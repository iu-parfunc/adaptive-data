package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentSkipListMap;

public class ObjectCreationCost {

	private int numNewObjects;
	private boolean warmUp;
	private String mapType;
	private ArrayList<ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>> scalableArray;
	private ArrayList<HybridIntMap<HybridIntMap<Integer>>> hybridArray;
	private ArrayList<PureIntMap<PureIntMap<Integer>>> pureArray;

	public ObjectCreationCost(String mapType, int numNewObjects) {

		this.mapType = mapType;

		System.out.println("ObjectCreationCost");

		switch (mapType) {
		case "pure":
			init(10000, true);
			benchPure();
			init(numNewObjects, false);
			benchPure();
			for (int i = 1; i < numNewObjects; i *= 10) {
				pureArray.get(i).put(new Integer(i), new PureIntMap<Integer>());
			}
			break;
		case "hybrid":
			init(10000, true);
			benchHybrid();
			init(numNewObjects, false);
			benchHybrid();
			for (int i = 1; i < numNewObjects; i *= 10) {
				hybridArray.get(i).put(new Integer(i),
						new HybridIntMap<Integer>());
			}
			break;
		case "scalable":
			init(10000, true);
			benchScalable();
			init(numNewObjects, false);
			benchScalable();
			for (int i = 1; i < numNewObjects; i *= 10) {
				scalableArray.get(i).put(new Integer(i),
						new ConcurrentSkipListMap<Integer, Integer>());
			}
			break;
		}
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
			hybridArray.add(new HybridIntMap<HybridIntMap<Integer>>());
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

	private void init(int numNewObjects, boolean warmUp) {
		switch (mapType) {
		case "pure":
			pureArray = new ArrayList<PureIntMap<PureIntMap<Integer>>>();
			break;
		case "hybrid":
			hybridArray = new ArrayList<HybridIntMap<HybridIntMap<Integer>>>();
			break;
		case "scalable":
			scalableArray = new ArrayList<ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>>>();
			break;
		}
		this.numNewObjects = numNewObjects;
		this.warmUp = warmUp;
		if (!warmUp) {
			System.out.println(String.format("creating %d new %s objects",
					numNewObjects, mapType));
		}
	}

	public static void main(String[] args) {

		new ObjectCreationCost(args[0], Integer.parseInt(args[1]));
	}
}
