package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.util.concurrent.ConcurrentSkipListMap;

public class ObjectCreationCost {

	int numNewObjects;
	boolean warmUp;

	public ObjectCreationCost() {

		System.out.println("ObjectCreationCost");
		System.out.println("Warming UP");
		numNewObjects = 1000000;
		warmUp = true;
		benchPure();
		benchScalable();
		benchHybrid();
		warmUp = false;
		System.out.println("Benchmarking");
		numNewObjects = 10000000;
		benchPure();
		benchScalable();
		benchHybrid();
	}

	private void benchScalable() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			new ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>();
		}
		long endTime = System.currentTimeMillis();
		if (!warmUp) {
			System.out.println("Scalable :" + (endTime - startTime));
		}
	}

	private void benchHybrid() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			new HybridIntMap<HybridIntMap<Integer>>(1);
		}
		long endTime = System.currentTimeMillis();
		if (!warmUp) {
			System.out.println("Hybrid :" + (endTime - startTime));
		}
	}

	private void benchPure() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < numNewObjects; i++) {
			new PureIntMap<PureIntMap<Integer>>();
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
