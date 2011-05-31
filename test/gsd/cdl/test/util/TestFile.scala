/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package gsd.cdl.test.util;

import java.io.File;
import org.junit.Ignore;

@Ignore
object TestFile {

    private val SEP = System.getProperty("file.separator");
    private val root = System.getProperty("user.dir") + SEP + "test" + SEP ;
    private val projectRoot = System.getProperty("user.dir") + SEP ;

    def get(file:String) : String = root + file

    def getRelativeToProjectRoot(file:String) : String = projectRoot + file

    def getAsFile(file:String) = new File(get(file))

    def  getRoot() : String = root;
}
