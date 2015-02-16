package benchmark;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class Inserter extends Thread {

	@SuppressWarnings("rawtypes")
	private Map map;
	private CountDownLatch startSignal, doneSignal;
	private int threadID;
	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	private AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap;
	private AtomicReference<IntTreePMap<?>> mutableIntTreeMap;

	private String mapValueType, concurrentMapType, benchmarkType;
	private double hotOrRandomKeyThreshold, hotKeyPercentage;
	private int insertionStratIndex, insertionEndIndex,
			numberOfRandomInsertionTries, hotColdKeyRangeMax;

	Random innerMapKeyValGen = new Random();
	Random hotOrRandomKeyGen = new Random();
	Random coldKeyGen = new Random();
	Random hotKeyGen = new Random();

	public Inserter(AtomicReference<IntTreePMap<?>> mutableIntTreeMap,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.SIMPLE_INSERTION_TO_MUTABLE_INT_TREE_MAP;
		this.mutableIntTreeMap = mutableIntTreeMap;
		this.mapValueType = mapValuetype;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		initializeSynchLatches(startSignal, doneSignal);
	}

	public Inserter(@SuppressWarnings("rawtypes") Map map,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {
		benchmarkType = Util.SIMPLE_INSERTION;
		this.map = map;
		this.mapValueType = mapValuetype;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		initializeSynchLatches(startSignal, doneSignal);
	}

	public Inserter(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap,
			int numberOfRandomInsertionTries, int hotColdKeyRangeMax,
			double hotKeyPercentage, double hotOrRandomKeyThreshold,
			CountDownLatch startSignal, CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerConcSkipListMap = outerConcSkipListMap;
		this.numberOfRandomInsertionTries = numberOfRandomInsertionTries;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.SKIP_LIST_MAP;
		initializeSynchLatches(startSignal, doneSignal);
	}

	public Inserter(
			ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap,
			int numberOfRandomInsertionTries, int hotColdKeyRangeMax,
			double hotKeyPercentage, double hotOrRandomKeyThreshold,
			CountDownLatch startSignal, CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerConcHashMap = outerConcHashMap;
		this.numberOfRandomInsertionTries = numberOfRandomInsertionTries;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.CONCURRENT_MAP;
		initializeSynchLatches(startSignal, doneSignal);
	}

	public Inserter(
			AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap,
			int numberOfRandomInsertionTries, int hotColdKeyRangeMax,
			double hotKeyPercentage, double hotOrRandomKeyThreshold,
			CountDownLatch startSignal, CountDownLatch doneSignal, int threadID) {

		this.threadID = threadID;
		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerMutableIntTreeMap = outerMutableIntTreeMap;
		this.numberOfRandomInsertionTries = numberOfRandomInsertionTries;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.MUTABLE_INT_TREE_MAP;
		initializeSynchLatches(startSignal, doneSignal);
	}

	private void initializeSynchLatches(CountDownLatch startSignal,
			CountDownLatch doneSignal) {
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void run() {

		try {

			/* Wait for start signal from the calling thread */
			startSignal.await();
			switch (benchmarkType) {
			case Util.SIMPLE_INSERTION:
				switch (mapValueType) {
				case Util.INT_TO_INT:
					for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
						map.put(new Integer(i), new Integer(i));
					}
					break;
				case Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT:
					for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
						map.put(new Integer(i), Collections
								.synchronizedMap(new HashMap<String, String>()));
					}
					break;
				default:
					break;
				}
				break;
			case Util.RANDOM_HOT_COLD:
				switch (concurrentMapType) {
				case Util.SKIP_LIST_MAP:
					randomInsertionToConcSKipListMapOfConcSKipListMap();
					break;
				case Util.CONCURRENT_MAP:
					randomInsertionToConcHahsMapOfConcHahsMap();
					break;
				case Util.MUTABLE_INT_TREE_MAP:
					randomInsertionToMutableIntTreeOfIntTreeValus();
					break;
				default:
					break;
				}
				break;
				
			case Util.SIMPLE_INSERTION_TO_MUTABLE_INT_TREE_MAP:
				switch (mapValueType) {
				case Util.INT_TO_INT:
					simpleInsertionBenchmarkONMutableIntTreeOfIntValus();
					break;
				case Util.INT_TO_MUTABLE_INT_TREE_MAP:
					simpleInsertionBenchmarkONMutableIntTreeOfIntTreeValus();
					break;
				default:
					break;
				}
				break;
			default:
				break;
			}

			/* Signal that this thread is done */
			doneSignal.countDown();

		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("unchecked")
	private void simpleInsertionBenchmarkONMutableIntTreeOfIntValus() {

		IntTreePMap<Integer> lastSnapSot;
		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer key = new Integer(i);
			Integer value = new Integer(i);
			lastSnapSot = (IntTreePMap<Integer>) mutableIntTreeMap.get();
			while (!mutableIntTreeMap.compareAndSet(lastSnapSot,
					lastSnapSot.plus(key, value))) {
				lastSnapSot = (IntTreePMap<Integer>) mutableIntTreeMap.get();
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void simpleInsertionBenchmarkONMutableIntTreeOfIntTreeValus() {

		IntTreePMap<IntTreePMap<Integer>> lastSnapSot;
		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
			
			Integer key = new Integer(i);
			IntTreePMap<Integer> value = IntTreePMap.empty();
			lastSnapSot = (IntTreePMap<IntTreePMap<Integer>>) mutableIntTreeMap
					.get();
			while (!mutableIntTreeMap.compareAndSet(lastSnapSot,
					lastSnapSot.plus(key, value))) {
				lastSnapSot = (IntTreePMap<IntTreePMap<Integer>>) mutableIntTreeMap
						.get();
			}
		}
	}

	private void randomInsertionToConcSKipListMapOfConcSKipListMap() {

		Integer randomKey;
		for (int i = 0; i < numberOfRandomInsertionTries; i++) {

			double hotOrColdKey = hotOrRandomKeyGen.nextDouble();
			if (hotOrColdKey < hotOrRandomKeyThreshold) {
				randomKey = new Integer(coldKeyGen.nextInt(hotColdKeyRangeMax));
			} else {
				randomKey = Util.getNextHotKey(hotKeyGen, 0,
						hotColdKeyRangeMax, hotKeyPercentage);
			}

			ConcurrentSkipListMap<Integer, Integer> newMap = new ConcurrentSkipListMap<Integer, Integer>();
			ConcurrentSkipListMap<Integer, Integer> innerMap = (ConcurrentSkipListMap<Integer, Integer>) outerConcSkipListMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			innerMap.put(nextRandomInnerMapKeyOrValue(),
					nextRandomInnerMapKeyOrValue());
		}
	}

	private void randomInsertionToConcHahsMapOfConcHahsMap() {

		Integer randomKey;
		for (int i = 0; i < numberOfRandomInsertionTries; i++) {

			double hotOrColdKey = hotOrRandomKeyGen.nextDouble();
			if (hotOrColdKey < hotOrRandomKeyThreshold) {
				randomKey = new Integer(coldKeyGen.nextInt(hotColdKeyRangeMax));

			} else {
				randomKey = Util.getNextHotKey(hotKeyGen, 0,
						hotColdKeyRangeMax, hotKeyPercentage);
			}

			ConcurrentHashMap<Integer, Integer> newMap = new ConcurrentHashMap<Integer, Integer>();
			ConcurrentHashMap<Integer, Integer> innerMap = (ConcurrentHashMap<Integer, Integer>) outerConcHashMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			innerMap.put(nextRandomInnerMapKeyOrValue(),
					nextRandomInnerMapKeyOrValue());
		}
	}

	private void randomInsertionToMutableIntTreeOfIntTreeValus() {

		Integer randomKey;
		for (int i = 0; i < numberOfRandomInsertionTries; i++) {

			double hotOrRandomKey = hotOrRandomKeyGen.nextDouble();
			if (hotOrRandomKey < hotOrRandomKeyThreshold) {
				randomKey = new Integer(coldKeyGen.nextInt(hotColdKeyRangeMax));
			} else {
				randomKey = Util.getNextHotKey(hotKeyGen, 0,
						hotColdKeyRangeMax, hotKeyPercentage);
			}
			putToOuterAndInnerMutableIntMap(outerMutableIntTreeMap, randomKey);
		}
	}

	private void putToOuterAndInnerMutableIntMap(
			AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap,
			Integer key) {

		AtomicReference<IntTreePMap<Integer>> newMap = new AtomicReference(
				IntTreePMap.empty());
		AtomicReference<IntTreePMap<Integer>> innerMutableIntTreeMap;

		IntTreePMap<AtomicReference<IntTreePMap<Integer>>> outerIntTreeMap = outerMutableIntTreeMap
				.get();
		innerMutableIntTreeMap = outerIntTreeMap.get(key);
		if (innerMutableIntTreeMap == null) {
			innerMutableIntTreeMap = newMap;
		}
		IntTreePMap<Integer> innerIntTreeMap = innerMutableIntTreeMap.get();

		Integer innerKey = nextRandomInnerMapKeyOrValue();
		Integer innerValue = nextRandomInnerMapKeyOrValue();

		try {

			while (!innerMutableIntTreeMap.compareAndSet(innerIntTreeMap,
					innerIntTreeMap.plus(innerKey, innerValue))
					|| !outerMutableIntTreeMap.compareAndSet(outerIntTreeMap,
							outerIntTreeMap.plus(key, innerMutableIntTreeMap))) {
				outerIntTreeMap = outerMutableIntTreeMap.get();
				innerMutableIntTreeMap = outerIntTreeMap.get(key);
				if (innerMutableIntTreeMap == null) {
					innerMutableIntTreeMap = newMap;
				}
				innerIntTreeMap = innerMutableIntTreeMap.get();
			}
			innerIntTreeMap = innerMutableIntTreeMap.get();

		} catch (NullPointerException e) {
			e.printStackTrace();
		}
	}

	private Integer nextRandomInnerMapKeyOrValue() {
		return new Integer(innerMapKeyValGen.nextInt(hotColdKeyRangeMax));
	}
}
