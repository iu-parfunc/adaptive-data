package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentSkipListMap;

public class ObjectCreationCost {

	private int numNewObjects;
	private boolean warmUp;
	private String mapType;
	private ArrayList<ConcurrentSkipListMap<Integer, Integer>> scalableArray;
	private ArrayList<HybridIntMap<Integer>> hybridArray;
	private ArrayList<PureIntMap<Integer>> pureArray;

	public ObjectCreationCost(String mapType, int numNewObjects) {

		this.mapType = mapType;

		System.out.println("ObjectCreationCost");

		long timeTaken = 0;
		switch (mapType) {
		case "pure":
			init(10000, true);
			benchPure();
			init(numNewObjects, false);
			timeTaken = benchPure();
			for (int i = 1; i < numNewObjects; i *= 10) {
				pureArray.get(i).put(new Integer(i), new Integer(i));
			}
			break;
		case "hybrid":
			init(10000, true);
			benchHybrid();
			init(numNewObjects, false);
			timeTaken = benchHybrid();
			for (int i = 1; i < numNewObjects; i *= 10) {
				hybridArray.get(i).put(new Integer(i), new Integer(i));
			}
			break;
		case "scalable":
			init(10000, true);
			benchScalable();
			init(numNewObjects, false);
			timeTaken = benchScalable();
			for (int i = 1; i < numNewObjects; i *= 10) {
				scalableArray.get(i).put(new Integer(i), new Integer(i));
			}
			break;
		}

		System.out.println(((double) timeTaken) / 1000.0
				+ " seconds to create " + numNewObjects + " " + mapType
				+ "<Integer,Integer> objects");
	}

	private long benchScalable() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) {
			scalableArray.add(new ConcurrentSkipListMap<Integer, Integer>());
		}
		for (int i = 0; i < numNewObjects; i++) {
			scalableArray.add(i % 10,
					new ConcurrentSkipListMap<Integer, Integer>());
		}
		long endTime = System.currentTimeMillis();
		return (endTime - startTime);
	}

	private long benchHybrid() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) {
			hybridArray.add(new HybridIntMap<Integer>());
		}
		for (int i = 0; i < numNewObjects; i++) {
			hybridArray.add(i % 10, new HybridIntMap<Integer>());
		}
		long endTime = System.currentTimeMillis();
		return (endTime - startTime);
	}

	private long benchPure() {

		long startTime = System.currentTimeMillis();
		for (int i = 0; i < 10; i++) {
			pureArray.add(new PureIntMap<Integer>());
		}
		for (int i = 0; i < numNewObjects; i++) {
			pureArray.add(i % 10, new PureIntMap<Integer>());
		}
		long endTime = System.currentTimeMillis();
		return (endTime - startTime);
	}

	private void init(int numNewObjects, boolean warmUp) {
		switch (mapType) {
		case "pure":
			pureArray = new ArrayList<PureIntMap<Integer>>();
			break;
		case "hybrid":
			hybridArray = new ArrayList<HybridIntMap<Integer>>();
			break;
		case "scalable":
			scalableArray = new ArrayList<ConcurrentSkipListMap<Integer, Integer>>();
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
