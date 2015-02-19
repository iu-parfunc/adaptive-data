package benchmark;

import hybrid_ds.HybridIntMap;

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
	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	private AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap;
	private HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap;

	private AtomicReference<IntTreePMap<Integer>> mutableIntTreeMapInt;
	private AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> mutableIntTreeMapInnerMap;
	private HybridIntMap<Integer> hybridIntMapInt;
	private HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMp;

	private CountDownLatch startSignal, doneSignal;

	private String mapValueType, concurrentMapType, benchmarkType;
	private double hotOrRandomKeyThreshold, hotKeyPercentage;
	private int insertionStratIndex, insertionEndIndex, hotColdKeyRangeMax;
	// private int numberOfRandomInsertionTries;

	private static Random randomGen = new Random();

	private int contentionCounter = 0, acceptibleContentionThreshold;

	private boolean offline = true;

	public Inserter(HybridIntMap<Integer> hybridIntMapInt,
			HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMp,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = Util.HYBRID_MAP;
		this.hybridIntMapInt = hybridIntMapInt;
		this.hybridIntMapInnerMp = hybridIntMapInnerMp;
		acceptibleContentionThreshold = (insertionStartIndex - insertionEndIndex) / 10;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype);
	}

	public Inserter(
			AtomicReference<IntTreePMap<Integer>> mutableIntTreeMapInt,
			AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> mutableIntTreeMapInnerMap,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = Util.MUTABLE_INT_TREE_MAP;
		this.mutableIntTreeMapInt = mutableIntTreeMapInt;
		this.mutableIntTreeMapInnerMap = mutableIntTreeMapInnerMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype);
	}

	public Inserter(@SuppressWarnings("rawtypes") Map map,
			int insertionStartIndex, int insertionEndIndex,
			String concurrentMapType, String mapValuetype,
			CountDownLatch startSignal, CountDownLatch doneSignal) {
		benchmarkType = Util.SIMPLE_INSERTION;
		this.map = map;
		this.concurrentMapType = concurrentMapType;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype);
	}

	public Inserter(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerConcSkipListMap = outerConcSkipListMap;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.SKIP_LIST_MAP;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex);
	}

	public Inserter(
			ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerConcHashMap = outerConcHashMap;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.CONCURRENT_MAP;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex);
	}

	public Inserter(
			AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerMutableIntTreeMap = outerMutableIntTreeMap;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.MUTABLE_INT_TREE_MAP;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex);
	}

	public Inserter(
			HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.outerHybridIntMapInnrMap = outerHybridIntMapInnrMap;
		acceptibleContentionThreshold = 1;// (insertionStartIndex -
											// insertionEndIndex) / 10;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
		concurrentMapType = Util.HYBRID_MAP;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex);
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
					switch (concurrentMapType) {
					case Util.MUTABLE_INT_TREE_MAP:
						simpleInsertionToMutableIntTreeOfInt();
						break;
					case Util.HYBRID_MAP:
						simpleInsertionToHybridMapOfInt();
						break;
					default:
						for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
							map.put(new Integer(i), new Integer(i));
						}
						break;
					}
					break;

				case Util.INT_TO_INNER_MAP:
					switch (concurrentMapType) {

					case Util.SKIP_LIST_MAP:
						for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
							map.put(new Integer(i),
									new ConcurrentSkipListMap<Integer, Integer>());
						}
						break;
					case Util.CONCURRENT_MAP:
						for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
							map.put(new Integer(i),
									new ConcurrentHashMap<Integer, Integer>());
						}
						break;
					case Util.SYNCHRONIZED_MAP:
						for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
							map.put(new Integer(i),
									Collections
											.synchronizedMap(new HashMap<Integer, Integer>()));
						}
						break;
					case Util.MUTABLE_INT_TREE_MAP:
						simpleInsertionToMutableIntTreeOfInnerMap();
						break;

					case Util.HYBRID_MAP:
						simpleInsertionToHybridMapOfInnerMap();
						break;
					}
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
				case Util.HYBRID_MAP:
					randomInsertionToHybrdiMapOfIntTreeValus();
					break;
				}
				break;
			}

			/* Signal that this thread is done */
			doneSignal.countDown();

		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void simpleInsertionToHybridMapOfInt() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer key = new Integer(i);
			Integer value = new Integer(i);
			hybridIntMapInt.tryPut(key, value);
			// while (!hybridIntMapInt.tryPut(key, value)) {
			// checkContention(hybridIntMapInt);
			// }
		}
	}

	private void simpleInsertionToHybridMapOfInnerMap() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer key = new Integer(i);
			HybridIntMap<Integer> value = new HybridIntMap<Integer>();
			hybridIntMapInnerMp.tryPut(key, value);
			// while (!hybridIntMapInnerMp.tryPut(key, value)) {
			// checkContention(hybridIntMapInnerMp);
			// }
		}
	}

	@SuppressWarnings("unchecked")
	private void simpleInsertionToMutableIntTreeOfInt() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer key = new Integer(i);
			Integer value = new Integer(i);
			IntTreePMap<Integer> lastSnapshot = mutableIntTreeMapInt.get();
			while (!mutableIntTreeMapInt.compareAndSet(lastSnapshot,
					lastSnapshot.plus(key, value))) {
				lastSnapshot = (IntTreePMap<Integer>) mutableIntTreeMapInt
						.get();
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void simpleInsertionToMutableIntTreeOfInnerMap() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer key = new Integer(i);
			AtomicReference<IntTreePMap<Integer>> value = new AtomicReference(
					IntTreePMap.empty());
			IntTreePMap<AtomicReference<IntTreePMap<Integer>>> lastSnapshot = mutableIntTreeMapInnerMap
					.get();
			while (!mutableIntTreeMapInnerMap.compareAndSet(lastSnapshot,
					lastSnapshot.plus(key, value))) {
				// System.out.println("CONTENTION");
				lastSnapshot = mutableIntTreeMapInnerMap.get();
			}
		}
	}

	private void randomInsertionToConcSKipListMapOfConcSKipListMap() {

		// for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
		for (int i = 0; i < (insertionEndIndex - insertionStratIndex); i++) {
			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentSkipListMap<Integer, Integer> newMap = new ConcurrentSkipListMap<Integer, Integer>();
			ConcurrentSkipListMap<Integer, Integer> innerMap = (ConcurrentSkipListMap<Integer, Integer>) outerConcSkipListMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue(innerMap.size() + 1);
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private void randomInsertionToHybrdiMapOfIntTreeValus() {
		// for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
		for (int i = 0; i < (insertionEndIndex - insertionStratIndex); i++) {
			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			HybridIntMap<Integer> newMap = new HybridIntMap<Integer>();
			HybridIntMap<Integer> innerMap = outerHybridIntMapInnrMap
					.get(randomKey);

			if (innerMap == null) {
				innerMap = newMap;
				outerHybridIntMapInnrMap.tryPut(randomKey, innerMap);
			}
			Integer innerKey = nextInnerMapKeyValue(innerMap.size() + 1);
			Integer innerValue = innerKey;
			innerMap.tryPut(innerKey, innerValue);
		}
	}

	private void randomInsertionToConcHahsMapOfConcHahsMap() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentHashMap<Integer, Integer> newMap = new ConcurrentHashMap<Integer, Integer>();
			ConcurrentHashMap<Integer, Integer> innerMap = (ConcurrentHashMap<Integer, Integer>) outerConcHashMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue(innerMap.size() + 1);
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private void randomInsertionToMutableIntTreeOfIntTreeValus() {

		Integer randomKey;
		// for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
		for (int i = 0; i < (insertionEndIndex - insertionStratIndex); i++) {
			randomKey = nextHotOrColdKeyOuterMap(i);
			putToOuterAndInnerMutableIntMap(outerMutableIntTreeMap, randomKey);
		}
	}

	private void putToOuterAndInnerMutableIntMap(
			AtomicReference<IntTreePMap<AtomicReference<IntTreePMap<Integer>>>> outerMutableIntTreeMap,
			Integer key) {

		AtomicReference<IntTreePMap<Integer>> newMap = new AtomicReference(
				IntTreePMap.empty());
		AtomicReference<IntTreePMap<Integer>> innerMutableIntTreeMap;
		Integer innerKey, innerValue;

		IntTreePMap<AtomicReference<IntTreePMap<Integer>>> outerIntTreeMap;
		IntTreePMap<Integer> innerIntTreeMap;
		try {

			do {
				outerIntTreeMap = outerMutableIntTreeMap.get();
				innerMutableIntTreeMap = outerIntTreeMap.get(key);
				while (innerMutableIntTreeMap == null) {

					innerMutableIntTreeMap = newMap;
					outerMutableIntTreeMap.compareAndSet(outerIntTreeMap,
							outerIntTreeMap.plus(key, innerMutableIntTreeMap));
					outerIntTreeMap = outerMutableIntTreeMap.get();
					innerMutableIntTreeMap = outerIntTreeMap.get(key);
				}

				innerIntTreeMap = innerMutableIntTreeMap.get();
				innerKey = nextInnerMapKeyValue(innerIntTreeMap.size() + 1);
				innerValue = innerKey;
				// System.out.println("CONTENTION");
			} while (!innerMutableIntTreeMap.compareAndSet(innerIntTreeMap,
					innerIntTreeMap.plus(innerKey, innerValue)));
			// || !outerMutableIntTreeMap.compareAndSet(outerIntTreeMap,
			// outerIntTreeMap.plus(key, innerMutableIntTreeMap)));
		} catch (NullPointerException e) {
			e.printStackTrace();
		}
	}

	// private void randomInsertionToHybrdiMapOfIntTreeValus() {
	// // for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
	// for (int i = 0; i < (insertionEndIndex - insertionStratIndex); i++) {
	// Integer randomKey = nextHotOrColdKeyOuterMap(i);
	// Integer innerKey, innerValue;
	// HybridIntMap<Integer> newMap = new HybridIntMap<Integer>();
	// HybridIntMap<Integer> innerMap;
	//
	// do {
	// innerMap = outerHybridIntMapInnrMap.get(randomKey);
	// if (innerMap == null) {
	// innerMap = newMap;
	// outerHybridIntMapInnrMap.tryPut(randomKey, innerMap);
	// System.out.println(randomKey + " on index "
	// + insertionEndIndex + " is NULL");
	// }
	// innerMap = outerHybridIntMapInnrMap.get(randomKey);
	// while (innerMap == null) {
	// innerMap = newMap;
	// outerHybridIntMapInnrMap.tryPut(randomKey, innerMap);
	// innerMap = outerHybridIntMapInnrMap.get(randomKey);
	// checkContention(outerHybridIntMapInnrMap);
	// }
	//
	// innerKey = nextInnerMapKeyValue(innerMap.size() + 1);
	// innerValue = innerKey;
	//
	// } while (!innerMap.tryPut(innerKey, innerValue));
	// }
	//
	// }

	private Integer nextInnerMapKeyValue() {
		return new Integer(randomGen.nextInt(hotColdKeyRangeMax));
	}

	private Integer nextInnerMapKeyValue(int offlineKey) {
		return new Integer(offlineKey);
	}

	private Integer nextHotOrColdKeyOuterMap(int coldkey) {

		Integer randomKey;
		double hotOrRandomKey = randomGen.nextDouble();
		if (hotOrRandomKey < hotOrRandomKeyThreshold) {
			if (offline) {
				randomKey = new Integer(coldkey);
				// System.out.println(coldkey);
			} else {
				randomKey = new Integer(randomGen.nextInt(hotColdKeyRangeMax));
			}
		} else {
			randomKey = Util.getNextHotKey(randomGen, 0, hotColdKeyRangeMax,
					hotKeyPercentage);
		}

		return randomKey;
	}

	private void checkContention(HybridIntMap hybridIntMap) {
		contentionCounter++;
		if (contentionCounter > acceptibleContentionThreshold) {
			hybridIntMap.contentionDetected();
		}
	}

	private void initializeFields(CountDownLatch startSignal,
			CountDownLatch doneSignal, int insertionStartIndex,
			int insertionEndIndex) {
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
	}

	private void initializeFields(CountDownLatch startSignal,
			CountDownLatch doneSignal, int insertionStartIndex,
			int insertionEndIndex, String mapValuetype) {
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		this.mapValueType = mapValuetype;
	}

}