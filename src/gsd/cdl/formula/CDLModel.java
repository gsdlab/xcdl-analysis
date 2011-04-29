package gsd.cdl.formula;

import java.util.HashSet;
import java.util.Set;

public class CDLModel {

    public String target;
    public int nodes;
    public int none;
    public int bool;
    public int booldata;
    public int data;
    public int packages;
    public int components;
    public int options;
    public int interfaces;

    public Set<String> features = new HashSet<String>();

    public int userChangeable;
    public int derived;
    public int withChangeableDataValue;

    public int mandatory;

    public int nodesWithAIconstraints;
    public int nodesWithReqsConstraints;

    public int dataNodesWhichAreTests;

    public int featuresInCommonWithI386;

}

