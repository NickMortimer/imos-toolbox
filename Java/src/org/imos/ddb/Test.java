/*
 * Copyright (c) 2009, eMarine Information Infrastructure (eMII) and Integrated 
 * Marine Observing System (IMOS).
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice, 
 *       this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright 
 *       notice, this list of conditions and the following disclaimer in the 
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the eMII/IMOS nor the names of its contributors 
 *       may be used to endorse or promote products derived from this software 
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.imos.ddb;

import java.lang.reflect.Field;
import java.util.List;

import org.imos.ddb.schema.*;

/**
 * Simple test case for DDB access.
 * 
 * @author Paul McCarthy <paul.mccarthy@csiro.au>
 */
public class Test {
  
  static void printObj(Object o) throws Exception {
    
    Field []fields = o.getClass().getDeclaredFields();
    
    for (Field f : fields) {
      
      if (f.isSynthetic()) continue;
      
      System.out.println(f.getName() + ": " + f.get(o));
    }
    System.out.println("------");
  }

  /**
   * @param args
   */
  public static void main(String[] args) {

    long startFreeMem, connectFreeMem, endFreeMem;
    DDB mdb = null;
    
    //String odbcArgs = "imos-ddb_bmorris";
    String odbcArgs = "/home/ggalibert/Documents/IMOS_toolbox/data_files_examples/NSW/OceanDB2015.mdb";
//    String odbcArgs = "/home/ggalibert/Documents/IMOS_toolbox/data_files_examples/AIMS/Paul_Rigby/OceanDB.mdb";
//    String odbcArgs = "/home/ggalibert/Documents/IMOS_toolbox/data_files_examples/AIMS/new_ddb/OceanDB_Unreplicated.mdb";

    String driver = "net.ucanaccess.jdbc.UcanaccessDriver";
    String mdbFile = "/home/ggalibert/Documents/IMOS_toolbox/data_files_examples/AIMS/Paul_Rigby/OceanDB.mdb";
    //String connection = "jdbc:ucanaccess://" + mdbFile + ";jackcessOpener=org.imos.ddb.CryptCodecOpener;SingleConnection=true";
    String connection = "jdbc:ucanaccess://" + mdbFile + ";jackcessOpener=org.imos.ddb.CryptCodecOpener";
    String user = "";
    String password = "";
    String[] jdbcArgs = new String[4];
    jdbcArgs[0] = driver;
    jdbcArgs[1] = connection;
    jdbcArgs[2] = user;
    jdbcArgs[3] = password;
    
    long startTime = System.currentTimeMillis();
//    startFreeMem = Runtime.getRuntime().totalMemory();
//    System.out.println("free mem: " + startFreeMem/1000000 + "Mb");
    long usedMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
    System.out.println("used mem: " + usedMem/1000000 + "Mb");
    
    try {
    	mdb = DDB.getDDB(odbcArgs);}
    	//mdb = DDB.getDDB(jdbcArgs[0], jdbcArgs[1], jdbcArgs[2], jdbcArgs[3]);}
    catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
 
    long connectTime = System.currentTimeMillis();
    System.out.println("Connection created in " + (connectTime - startTime)/1000 + " seconds.");
    
//    connectFreeMem = Runtime.getRuntime().totalMemory();
//    System.out.println("free mem: " + connectFreeMem/1000000 + "Mb");
//    System.out.println("lost mem: " + (startFreeMem - connectFreeMem)/1000000 + "Mb");
    usedMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
    System.out.println("used mem: " + usedMem/1000000 + "Mb");
    
    try {

    	List<FieldTrip> trips = mdb.executeQuery("FieldTrip", null, null);
    	for (FieldTrip t : trips) printObj(t);
    	
    	List<DeploymentData> deps = mdb.executeQuery("DeploymentData", null, null);
    	for (DeploymentData d : deps) printObj(d);
    	
    	List<Sites> capeSites = mdb.executeQuery("Sites", null, null);
    	for (Sites s : capeSites) printObj(s);
    	
    	List<CTDData> casts = mdb.executeQuery("CTDData", null, null);
    	for (CTDData d : casts) printObj(d);
    	
    	List<Instruments> inst = mdb.executeQuery("Instruments", null, null);
    	for (Instruments d : inst) printObj(d);
    	
    	List<Sensors> sens = mdb.executeQuery("Sensors", null, null);
    	for (Sensors d : sens) printObj(d);
    	
    	List<InstrumentSensorConfig> instSens = mdb.executeQuery("InstrumentSensorConfig", null, null);
    	for (InstrumentSensorConfig d : instSens) printObj(d);
    }
    catch (Exception e) {e.printStackTrace();}
    
    long stopTime = System.currentTimeMillis();
    System.out.println("Query performed in " + (stopTime - connectTime)/1000 + " seconds.");
    
//    endFreeMem = Runtime.getRuntime().totalMemory();
//    System.out.println("free mem: " + endFreeMem/1000000 + "Mb");
//    System.out.println("lost mem: " + (connectFreeMem - endFreeMem)/1000000 + "Mb");
    usedMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
    System.out.println("used mem: " + usedMem/1000000 + "Mb");
  }
}
