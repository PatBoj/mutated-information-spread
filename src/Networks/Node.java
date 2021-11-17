package Networks;
import java.util.ArrayList;

import Dynamics.Message;

public class Node 
{
	// ~ DATA FIELDS ~
	
	/***** NETWORK *****/
	
	// List of all connections to this particular node
	private ArrayList<Link> links;
	
	// Number of component that this node belongs to
	// Value dosen't mean anything, it's only label of component
	private int component; 
	
	// Temporary number that helps to count the distance between nodes
	private int distance;
	
	/***** DYNAMICS *****/
	
	// Opinion of the node
	// single value stands for opinion of one subject
	private int[] nodeOpinion;
	
	// Messages that this node shared
	private ArrayList<Message> messages;
	
	// Messages that will appear on this node's dashboard
	private ArrayList<Message> dashboard;
	
	// Cosine threshold
	private double cosineThreshold;
	
	// ID's of shared messages
	private ArrayList<Integer> ids;
	
	// ~ CONSTRUCTORS ~ 
	// Constructor #1
	Node(int[] nodeOpinion) {
		// Errors and exceptions
		for(double opinion : nodeOpinion)
			if(Math.abs(opinion) != 1 && opinion != 0)
				throw new Error("Node opinion must be equal -1, 0 or 1.");
		
		this.nodeOpinion = nodeOpinion;
		ids = new ArrayList<Integer>();
		links = new ArrayList<Link>();
		messages = new ArrayList<Message>();
		dashboard = new ArrayList<Message>();
		component = -1;
		distance = -1;
		cosineThreshold = -2;
	}
	
	// Copy constructor
	Node(Node node) {
		this.nodeOpinion = node.nodeOpinion.clone();
		links = new ArrayList<Link>();
		for(Link link : node.links)
			links.add(new Link(link));
		this.component = node.component;
		this.distance = node.distance;
		messages = new ArrayList<Message>();
		for(Message msg : node.messages)
			messages.add(new Message(msg));
		dashboard = new ArrayList<Message>();
		for(Message msg : node.dashboard)
			dashboard.add(new Message(msg));
		this.cosineThreshold = node.cosineThreshold;
		ids = new ArrayList<Integer>();
		for(int id : node.ids)
			ids.add(id);
	}
	
	// Default constructor
	Node() {this(new int[] {0});}
	
	/*******************/
	/***** NETWORK *****/
	/*******************/
	
	// ~ METHODS ~
	// Adds link without checking if such connection exists
	void addLinkNC(int node1, int node2) {links.add(new Link(node1, node2));}
	void addLinkNC(Link link) {links.add(link);}
	
	// Check if connection between node1 and node2
	// returns -1 if there is no connection
	// returns position of element on the links list if there is a connection
	int checkLink(int node1, int node2) { 	
		for(int i=0; i<links.size(); i++)
			if(links.get(i).checkConnection(node1, node2)) return i;
		return -1;
	}
	
	// Check if connection exists
	int checkLink(Link link) {
		return checkLink(link.getConnection()[0], link.getConnection()[1]);
	}
	
	// Adds link
	// returns true if adds connection
	// returns false if dosen't adds connection (because such connection already exists)
	boolean addLink(int node1, int node2) {
		if(checkLink(node1, node2) == -1) {links.add(new Link(node1, node2)); return true;}
		else return false;
	}
	
	boolean addLink(Link link) {
		if(checkLink(link) == -1) {links.add(link); return true;}
		else return false;
	}	
	
	// Removes link
	// returns true if removes connection
	// returns false if doesn't removes connection (because such connection doesn't exists)
	boolean deleteLink(int node1, int node2) {
		// "i" is the position of the link in the connection list
		int i = checkLink(node1, node2);
		if(i != -1) {links.remove(i); return true;}
		else return false;
	}
	
	boolean deleteLink(Link link) {
		return deleteLink(link.getConnection()[0], link.getConnection()[1]);
	}
	
	// Deletes all elements in the links array
	void clearLinks() {links.clear();}
	
	// ~ GETTERS ~
	int getNodeDegree() {return links.size();}
 	Link getLink(int i) {return links.get(i);}
	ArrayList<Link> getLinks() {return links;}
	public int[] getConnection(int i) {return links.get(i).getConnection();}
	int getComponentAssociation() {return component;}
	public int getDistance() {return distance;}
	
	// ~ SET ~
	void setComponentAssociation(int c) {component = c;}
	void setDistance(int d) {distance = d;}
	
	/********************/
	/***** DYNAMICS *****/
	/********************/
	
	// ~ METHODS ~
	public void clearDashboard() {dashboard.clear();}
	public void deleteFromDashboard(int index) {dashboard.remove(index);}
	
	// ~ SET ~
	public void setNodeOpinion(int[] nodeOpinion) {this.nodeOpinion = nodeOpinion.clone();}
	public void setOneNodeOpinion(int index, int value) {
		if(value != -1 & value != 0 & value != 1)
			throw new Error("Opinion element must be -1, 0 or 1.");
		nodeOpinion[index] = value;
	}
	public void setMessage(Message msg) {messages.add(msg); ids.add(msg.getId());}
	public void setSharedMessage(Message msg) {dashboard.add(msg);}
	public void setThreshold(double cosineThreshold) {this.cosineThreshold = cosineThreshold;}
		
	// ~ GETTERS ~
	public int[] getNodeOpinion() {return nodeOpinion;}
	public ArrayList<Message> getAllNodeMessages() {return messages;}
	public ArrayList<Message> getNodeDashboard() {return dashboard;}
	public ArrayList<Integer> getSharedIds() {return ids;}
	public double getCosineThreshold() {return cosineThreshold;}
}
