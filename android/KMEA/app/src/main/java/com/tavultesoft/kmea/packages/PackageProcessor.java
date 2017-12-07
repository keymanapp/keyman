package com.tavultesoft.kmea.packages;

import java.io.File;

/**
 * Created by joshua on 12/7/2017.
 */

public class PackageProcessor {
  public static void /*eventually -> Package*/ processKMP(File path) {
    System.out.println("File size: " + path.length());
    System.out.println("Absolute path: " + path.getAbsolutePath());
  }
}
