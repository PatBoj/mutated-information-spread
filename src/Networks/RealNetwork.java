package Networks;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

import ProgramingTools.Tools;

public class RealNetwork extends Network {
	// ~ CONSTRUCTORS ~
	// Constructor #1
	public RealNetwork(String filePath) {
		ArrayList<int[]> connections = new ArrayList<int[]>();
		String[] tempLink;
		int[] flatTable;
		
		try {
			File myObj = new File(filePath);
			Scanner myReader = new Scanner(myObj);
			
			while (myReader.hasNextLine()) {
				String data = myReader.nextLine();
				tempLink = data.split("\t");
				connections.add(new int[] {Integer.parseInt(tempLink[0]), 
						Integer.parseInt(tempLink[1])});
		        }
			
			myReader.close();
			
			} catch (FileNotFoundException e) {
				System.out.println("An error occurred.");
				e.printStackTrace();
			}
		
		flatTable = new int[connections.size() * 2];
		
		for(int i=0; i<connections.size(); i++) {
			for(int j=0; j<2; j++) {
				flatTable[2*i + j] = connections.get(i)[j];
			}
		}
		
		N = Tools.getMaximum(flatTable) + 1;
		E = 0;
		averageDegree = 0;
		nodes = new ArrayList<Node>(N);
		for (int i = 0; i < N; i++)
			nodes.add(new Node());
		globalLinks = new ArrayList<Link>();
		components = new ArrayList<Integer>();
		rnd = new Random();
		typeOfTopology = "custom";
		
		for(int i=0; i<connections.size(); i++)
			addLink(connections.get(i)[0], connections.get(i)[1]);
	}
		
	// Copy constructor
	public RealNetwork(RealNetwork network) {
		super(network);
	}
}
