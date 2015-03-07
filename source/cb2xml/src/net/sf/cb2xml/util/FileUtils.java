/*************************************************************
 * This file is part of CB2XML.  
 * See the file "LICENSE" for copyright information and the
 * terms and conditions for copying, distribution and
 * modification of CB2XML.
 *************************************************************
 */

package net.sf.cb2xml.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

/**
* quick and easy file utilities useful for debugging / logging
* @author Peter Thomas
*/
public class FileUtils {
  public static StringBuffer readFile(String fileName) {
    File file = new File(fileName);
    FileInputStream fis = null;
    BufferedReader buffer = null;
    StringBuffer sb = new StringBuffer();
    String s = null;
    try {
      fis = new FileInputStream(file);
      buffer = new BufferedReader(new InputStreamReader(fis));
      while ( (s = buffer.readLine()) != null) {
        sb.append(s);
      }
    }
    catch (Exception e) {
      e.printStackTrace();
      return null;
    }
    finally {
      if (fis != null) {
        try {
          fis.close();
        }
        catch (IOException e) {
          ;
        }
      }
    }
    return sb;
  }

  public static void writeFile(String content, String fileName, boolean append) {
    FileWriter writer = null;
    try {
      writer = new FileWriter(fileName, append);
      writer.write(content);
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    finally {
      if (writer != null) {
        try {
          writer.close();
        }
        catch (Exception e) {}
        ;
      }
    }
  }

}