package Main;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import Dynamics.Dynamics;
import Networks.Network;
import Networks.RandomGraph;
import Networks.ScaleFreeNetwork;
import ProgramingTools.Mailer;
import ProgramingTools.Time;
import ProgramingTools.Tools;

public class Experiments implements Runnable {
	private int id;
	
	private int N;
	private int k;
	private int timeSteps;
	private int dimOpinion;
	private double pEdit;
	private double pNewMessage;
	private int realisations;
	private double threshold;
	private double sim;
	private boolean competition;
	
	private String topologyType;
	private Network net;
	
	Experiments(int id, double tau, String topologyType, double pEdit, int realisations, double sim) {
		this.id = id;
		
		//N = 50000;
		N = 600;
		k = 6;
		timeSteps = 50000;
		//timeSteps = 1000000;
		dimOpinion = 100;
		this.pEdit = pEdit;
		pNewMessage = 0.1;
		this.realisations = realisations;
		threshold = tau;
		this.sim = sim;
		this.topologyType = topologyType;
		net = new Network();
		competition = true;
	}

	public void run() {		
		if(topologyType.equals("ER"))
			net = new RandomGraph(N, (double)k/(N-1));
		else if(topologyType.equals("BA"))
			net = new ScaleFreeNetwork(N, k/2);
		
		Dynamics dyn = new Dynamics(net, dimOpinion, pNewMessage, pEdit, threshold);
		String folder = "results/data/";
		Save s = new Save(folder + net.getTopologyType() + "_" + id + "_tau_" + Tools.convertToString(threshold) + (pEdit == 0 ? "_non_" : "_all_") + Tools.convertToString(sim, 1) + ".txt");
		
		dyn.setInitialOpinions();
		dyn.saveParameters(s, realisations, timeSteps, competition);
		
		//dyn.saveHeader(s);
		dyn.run(timeSteps, s, competition);
		dyn.saveData(s);
		s.closeWriter();
		
		Time.count();
		Time.globalProgress();
	}
	
	public static void runExperiment() throws InterruptedException {
		int N = 10;
		double dt = 0.04;
		int n = (int)(2/dt+1);
		
		double alpha = 0.2;
		
		//double[] tau = new double[n];
		//for(int i=0; i<n; i++)
		//	tau[i] = -1 + i * dt;
		
		
		double[] tau = new double[] {-0.4, 0.2, 0.8};
		
		//double[] sim = new double[] {0.0, 0.2, 0.4, 0.6};
		//double[] sim = new double[] {0.0};
		
		Time.setMaxIterations(4 * tau.length * N);
		
		ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() -1);
		//ExecutorService executor = Executors.newFixedThreadPool(1);
		
		for(int i=0; i<tau.length; i++)
			for(int j=0 ; j<N; j++)
				executor.execute(new Experiments(j+1, tau[i], "ER", 0, N, 0));
		
		for(int i=0; i<tau.length; i++) 
			for(int j=0 ; j<N; j++)
				executor.execute(new Experiments(j+1, tau[i], "ER", alpha, N, 0));
		
		for(int i=0; i<tau.length; i++) 
			for(int j=0 ; j<N; j++)
				executor.execute(new Experiments(j+1, tau[i], "BA", 0, N, 0));
		
		for(int i=0; i<tau.length; i++) 
			for(int j=0 ; j<N; j++)
				executor.execute(new Experiments(j+1, tau[i], "BA", alpha, N, 0));
		
		executor.shutdown();
		executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		
		//Mailer.send();
		//Time.speek();
	}
	
	public static void test() throws InterruptedException {
		Time t = new Time();
		int N = 2;
		double dt = 0.5;
		int n = (int)(2/dt+1);
		
		double[] tau = new double[n];
		for(int i=0; i<n; i++)
			tau[i] = -1 + i * dt;
		
		double[] sim = new double[] {0, 0.2, 0.4, 0.6};
		
		Time.setMaxIterations(n * N * 4 * 4);
		
		ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		
		for(int k=0; k<4; k++) {
			for(int i=0; i<n; i++)
				for(int j=0 ; j<N; j++)
					executor.execute(new Experiments(j+1, tau[i], "ER", 0, N, sim[k]));
		
			for(int i=0; i<n; i++) 
				for(int j=0 ; j<N; j++)
					executor.execute(new Experiments(j+1, tau[i], "ER", 0.05, N, sim[k]));
		
			for(int i=0; i<n; i++) 
				for(int j=0 ; j<N; j++)
					executor.execute(new Experiments(j+1, tau[i], "BA", 0, N, sim[k]));
		
			for(int i=0; i<n; i++) 
				for(int j=0 ; j<N; j++)
					executor.execute(new Experiments(j+1, tau[i], "BA", 0.05, N, sim[k]));
		}
		
		executor.shutdown();
		executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		
		//Mailer.send();
		t.printDuration();
		Time.speek();
	}
}
