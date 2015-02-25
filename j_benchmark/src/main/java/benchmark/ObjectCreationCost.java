package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentSkipListMap;

public class ObjectCreationCost {

	int numNewObjects;
	boolean warmUp;
	private ArrayList<ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>> scalableArray = new ArrayList<ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>>();
	private ArrayList<HybridIntMap<HybridIntMap<Integer>>> hybridArray = new ArrayList<HybridIntMap<HybridIntMap<Integer>>>();
	private ArrayList<PureIntMap<PureIntMap<Integer>>> pureArray = new ArrayList<PureIntMap<PureIntMap<Integer>>>();

	public ObjectCreationCost() {

		System.out.println("ObjectCreationCost");
		System.out.println("Warming UP");
		numNewObjects = 1000;
		warmUp = true;
		benchPure();
		benchScalable();
		benchHybrid();
		warmUp = false;
		System.out.println("Benchmarking");
		numNewObjects = 10000;
		System.out.println(String.format("creating %d new objects",
				numNewObjects));
		benchPure();
		benchScalable();
		benchHybrid();

		scalableArray = new ArrayList<ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>>();
		hybridArray = new ArrayList<HybridIntMap<HybridIntMap<Integer>>>();
		pureArray = new ArrayList<PureIntMap<PureIntMap<Integer>>>();
	}

	private void benchScalable() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			scalableArray
					.add(new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>());
		}
		long endTime = System.currentTimeMillis();
		if (!warmUp) {
			System.out.println("Scalable :" + (endTime - startTime));
		}
	}

	private void benchHybrid() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			hybridArray.add(new HybridIntMap<HybridIntMap<Integer>>(1));
		}
		long endTime = System.currentTimeMillis();
		if (!warmUp) {
			System.out.println("Hybrid :" + (endTime - startTime));
		}
	}

	private void benchPure() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			pureArray.add(new PureIntMap<PureIntMap<Integer>>());
		}
		long endTime = System.currentTimeMillis();
		if (!warmUp) {
			System.out.println("Pure :" + (endTime - startTime));
		}
	}

	public static void main(String[] args) {
		new ObjectCreationCost();
	}
}
