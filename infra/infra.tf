# Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

terraform {
  backend "gcs" {
    bucket = "da-dev-gcp-daml-language-tfstate"
    prefix = "ghc-lib"
  }
}

provider "google" {
  project = "da-dev-gcp-daml-language"
  region  = "us-east4"
  version = "3.5"
}

data "google_project" "current" {
  project_id = local.project
}

locals {
  labels = {
    cost-allocation = "daml-language"
    host-group      = "buildpipeline"
    infra-owner     = "daml-language"
    managed         = "true"

    # default the target name to be the name of the folder
    target = basename(path.module)
  }

  project = "da-dev-gcp-daml-language"
  region  = "us-east4"

  // maintained by DA security
  ssl_certificate = "https://www.googleapis.com/compute/v1/projects/da-dev-gcp-daml-language/global/sslCertificates/da-ext-wildcard"
}

resource "google_storage_bucket" "default" {
  project = local.project
  name    = "da-ghc-lib"
  labels  = local.labels

  # SLA is enough for a cache and is cheaper than MULTI_REGIONAL
  # see https://cloud.google.com/storage/docs/storage-classes
  storage_class = "REGIONAL"

  # Use a normal region since the storage_class is regional
  location = local.region
}

resource "google_storage_bucket_acl" "default" {
  bucket      = google_storage_bucket.default.name
  default_acl = "publicread"
  role_entity = [
    "OWNER:project-owners-${data.google_project.current.number}",
    "OWNER:project-editors-${data.google_project.current.number}",
    "READER:project-viewers-${data.google_project.current.number}",
    "READER:allUsers",
  ]
}


resource "google_service_account" "writer" {
  account_id   = "da-ghc-lib-ci-writer"
  display_name = "ghc-lib Writer"
  project      = local.project
}

resource "google_storage_bucket_iam_member" "bucket-ci-rw" {
  bucket = google_storage_bucket.default.name

  # https://cloud.google.com/storage/docs/access-control/iam-roles
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.writer.email}"
}

output bucket_ip {
  description = "The external IP assigned to the global fowarding rule."
  value       = google_compute_global_address.default.address
}

resource "google_compute_backend_bucket" "default" {
  project     = local.project
  name        = "da-ghc-lib-backend"
  bucket_name = google_storage_bucket.default.name
  enable_cdn  = true
}

resource "google_compute_global_address" "default" {
  project    = local.project
  name       = "da-ghc-lib-address"
  ip_version = "IPV4"
}

resource "google_compute_url_map" "default" {
  project         = local.project
  name            = "da-ghc-lib"
  default_service = google_compute_backend_bucket.default.self_link
}

resource "google_compute_target_http_proxy" "default" {
  project = local.project
  name    = "da-ghc-lib-http-proxy"
  url_map = google_compute_url_map.default.self_link
}

resource "google_compute_global_forwarding_rule" "http" {
  project    = local.project
  name       = "da-ghc-lib-http"
  target     = google_compute_target_http_proxy.default.self_link
  ip_address = google_compute_global_address.default.address
  port_range = "80"
  depends_on = [google_compute_global_address.default]
}

resource "google_compute_target_https_proxy" "default" {
  project          = local.project
  name             = "da-ghc-lib-https-proxy"
  url_map          = google_compute_url_map.default.self_link
  ssl_certificates = [local.ssl_certificate]
}

resource "google_compute_global_forwarding_rule" "https" {
  project    = local.project
  name       = "da-ghc-lib-https"
  target     = google_compute_target_https_proxy.default.self_link
  ip_address = google_compute_global_address.default.address
  port_range = "443"
  depends_on = [google_compute_global_address.default]
}
