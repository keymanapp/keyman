package com.keyman.engine.cloud;

public class DownloadManagerDisabledException extends RuntimeException {
  DownloadManagerDisabledException() {
    super("System service DownloadManager is not available and cannot facilitate downloads or queries.");
  }
}