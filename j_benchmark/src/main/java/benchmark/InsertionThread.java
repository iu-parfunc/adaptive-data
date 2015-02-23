package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

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

public class InsertionThread extends Thread {

	@SuppressWarnings("rawtypes")
	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	private ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	private PureIntMap<PureIntMap<Integer>> outerPureIntMap;

	private HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap;

	private Map map;
	private PureIntMap<Integer> pureIntMapInt;
	private PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap;
	private HybridIntMap<Integer> hybridIntMapInt;
	private HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMap;

	private CountDownLatch startSignal, doneSignal;

	private String mapValueType, concurrentMapType, benchmarkType;
	private double coldKeyProbability, hotKeyPercentage;
	private int insertionStratIndex, insertionEndIndex, hotColdKeyRangeMax;

	private static Random randomGen = new Random();

	private boolean offline = true;

	public InsertionThread(HybridIntMap<Integer> hybridIntMapInt,
			HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMp,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = Util.HYBRID_MAP;
		this.hybridIntMapInt = hybridIntMapInt;
		this.hybridIntMapInnerMap = hybridIntMapInnerMp;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype, 0, 0, 0);
	}

	public InsertionThread(PureIntMap<Integer> pureIntMapInt,
			PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = Util.PURE_MAP;
		this.pureIntMapInt = pureIntMapInt;
		this.pureIntMapInnerMap = pureIntMapInnerMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype, 0, 0, 0);
	}

	public InsertionThread(@SuppressWarnings("rawtypes") Map map,
			int insertionStartIndex, int insertionEndIndex,
			String concurrentMapType, String mapValuetype,
			CountDownLatch startSignal, CountDownLatch doneSignal) {
		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = concurrentMapType;
		this.map = map;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype, 0, 0, 0);
	}

	public InsertionThread(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double coldKeyProbability, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		concurrentMapType = Util.SKIP_LIST_MAP;
		this.outerConcSkipListMap = outerConcSkipListMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, null, coldKeyProbability, hotKeyPercentage,
				hotColdKeyRangeMax);
	}

	public InsertionThread(
			ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double coldKeyProbability, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		concurrentMapType = Util.CONCURRENT_MAP;
		this.outerConcHashMap = outerConcHashMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, null, coldKeyProbability, hotKeyPercentage,
				hotColdKeyRangeMax);
	}

	public InsertionThread(PureIntMap<PureIntMap<Integer>> outerPureIntMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double coldKeyProbability, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		concurrentMapType = Util.PURE_MAP;
		this.outerPureIntMap = outerPureIntMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, null, coldKeyProbability, hotKeyPercentage,
				hotColdKeyRangeMax);
	}

	public InsertionThread(
			HybridIntMap<HybridIntMap<Integer>> outerHybridIntMapInnrMap,
			int insertionStartIndex, int insertionEndIndex,
			int hotColdKeyRangeMax, double hotKeyPercentage,
			double coldKeyProbability, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		concurrentMapType = Util.HYBRID_MAP;
		this.outerHybridIntMapInnrMap = outerHybridIntMapInnrMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, null, coldKeyProbability, hotKeyPercentage,
				hotColdKeyRangeMax);
	}

	@Override
	public void run() {

		try {

			/* Wait for start signal from the calling thread */
			startSignal.await();
			switch (benchmarkType) {

			case Util.SIMPLE_INSERTION:
				switch (mapValueType) {
				case Util.INT_TO_INT:
					simpleInsertToMapOfInt();
					break;
				case Util.INT_TO_INNER_MAP:
					simpleInsertToMapOfInnerMap();
				}
				break;

			case Util.RANDOM_HOT_COLD:
				switch (concurrentMapType) {

				case Util.SKIP_LIST_MAP:
					randomInsertion_SKIP_LIST_WITH_INNER_CONC_SKIP_LIST_MAP();
					break;
				case Util.CONCURRENT_MAP:
					randomInsertion_CONCURRENT_HASH_MAP_WITH_INNER_CONC_HASH_MAP();
					break;
				case Util.HYBRID_MAP:
					randomInsertion_HYBRID_WITH_INNER_HYBRID_MAP();
					break;
				case Util.PURE_MAP:
					randomInsertion_PURE_WITH_INNER_PURE_MAP();
					break;
				}
			}

			/* Signal that this thread is done */
			doneSignal.countDown();

		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void simpleInsertToMapOfInt() {

		switch (concurrentMapType) {
		case Util.PURE_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
				pureIntMapInt.put(new Integer(i), new Integer(i));
			}
			break;
		case Util.HYBRID_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
				hybridIntMapInt.put(new Integer(i), new Integer(i));
			}
			break;
		default:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
				map.put(new Integer(i), new Integer(i));
			}
			break;
		}

	}

	private void simpleInsertToMapOfInnerMap() {

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
				map.put(new Integer(i), Collections
						.synchronizedMap(new HashMap<Integer, Integer>()));
			}
			break;
		case Util.HYBRID_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

				hybridIntMapInnerMap.put(new Integer(i),
						new HybridIntMap<Integer>());
			}
			break;

		case Util.PURE_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

				pureIntMapInnerMap.put(new Integer(i),
						new PureIntMap<Integer>());
			}
			break;
		}
	}

	private void randomInsertion_SKIP_LIST_WITH_INNER_CONC_SKIP_LIST_MAP() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentSkipListMap<Integer, Integer> newMap = new ConcurrentSkipListMap<Integer, Integer>();
			ConcurrentSkipListMap<Integer, Integer> innerMap = (ConcurrentSkipListMap<Integer, Integer>) outerConcSkipListMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue();
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private void randomInsertion_HYBRID_WITH_INNER_HYBRID_MAP() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			HybridIntMap<Integer> newMap = new HybridIntMap<Integer>();
			HybridIntMap<Integer> innerMap = outerHybridIntMapInnrMap
					.putIfAbsent(randomKey, newMap);

			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue();
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}
	
	private void randomInsertion_PURE_WITH_INNER_PURE_MAP() {

		Integer randomKey;
		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
			randomKey = nextHotOrColdKeyOuterMap(i);
			PureIntMap<Integer> newMap = new PureIntMap<>();
			PureIntMap<Integer> innerMap = outerPureIntMap.putIfAbsent(
					randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue();
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);

		}
	}

	private void randomInsertion_CONCURRENT_HASH_MAP_WITH_INNER_CONC_HASH_MAP() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentHashMap<Integer, Integer> newMap = new ConcurrentHashMap<Integer, Integer>();
			ConcurrentHashMap<Integer, Integer> innerMap = (ConcurrentHashMap<Integer, Integer>) outerConcHashMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue();
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private Integer nextInnerMapKeyValue(int offlineKey) {
		return new Integer(offlineKey);
	}

	private Integer nextInnerMapKeyValue() {

		return new Integer(randomGen.nextInt());
	}

	private Integer nextHotOrColdKeyOuterMap(int coldkey) {

		Integer randomKey;
		double hotOrRandomKey = randomGen.nextDouble();
		if (hotOrRandomKey < coldKeyProbability) {
			if (offline) {
				randomKey = new Integer(coldkey);
			} else {
				randomKey = new Integer(randomGen.nextInt(hotColdKeyRangeMax));
			}
		} else {
			randomKey = Util.getNextHotKey(randomGen, 0, hotColdKeyRangeMax,
					hotKeyPercentage);
		}

		return randomKey;
	}

	private void initializeFields(CountDownLatch startSignal,
			CountDownLatch doneSignal, int insertionStartIndex,
			int insertionEndIndex, String mapValuetype,
			double coldKeyProbability, double hotKeyPercentage,
			int hotColdKeyRangeMax) {
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		this.mapValueType = mapValuetype;
		this.coldKeyProbability = coldKeyProbability;
		this.hotKeyPercentage = hotKeyPercentage;
		this.hotColdKeyRangeMax = hotColdKeyRangeMax;
	}

}