package benchmark;

import hybrid_ds.HybridIntMap;
import hybrid_ds.PureIntMap;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentHashMap;

public class InsertionThread extends Thread {

	private ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	private ConcurrentSkipListMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	private ConcurrentSkipListMap<Integer, PureIntMap<Integer>> outerPureIntMap;
	private ConcurrentSkipListMap<Integer, HybridIntMap<Integer>> outerHybridIntMapInnrMap;

	private Map<Integer, Integer> mapInt;
	private Map<Integer, Map<Integer, Integer>> mapInnerMap;
	private PureIntMap<Integer> pureIntMapInt;
	private PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap;
	private HybridIntMap<Integer> hybridIntMapInt;
	private HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMap;

	private CountDownLatch startSignal, doneSignal;

	private String mapValueType, concurrentMapType, benchmarkType;
	private double hotRatio;
	private int insertionStratIndex, insertionEndIndex, coldKeyRangeMax,
			numHotKey;

	private static Random randomGen = new Random();

	private boolean offlineOuterColdKeys = false;
	private boolean offlineInnerKeys = false;

	public InsertionThread(Map<Integer, Integer> mapInt,
			Map<Integer, Map<Integer, Integer>> mapInnerMap,
			PureIntMap<Integer> pureIntMapInt,
			PureIntMap<PureIntMap<Integer>> pureIntMapInnerMap,
			HybridIntMap<Integer> hybridIntMapInt,
			HybridIntMap<HybridIntMap<Integer>> hybridIntMapInnerMp,
			int insertionStartIndex, int insertionEndIndex,
			String concurrentMapType, String mapValuetype,
			CountDownLatch startSignal, CountDownLatch doneSignal) {
		benchmarkType = Util.SIMPLE_INSERTION;
		this.concurrentMapType = concurrentMapType;
		this.mapInt = mapInt;
		this.mapInnerMap = mapInnerMap;
		this.pureIntMapInt = pureIntMapInt;
		this.pureIntMapInnerMap = pureIntMapInnerMap;
		this.hybridIntMapInt = hybridIntMapInt;
		this.hybridIntMapInnerMap = hybridIntMapInnerMp;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, mapValuetype, 0, 0, 0);
	}

	public InsertionThread(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap,
			ConcurrentSkipListMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap,
			ConcurrentSkipListMap<Integer, PureIntMap<Integer>> outerPureIntMap,
			ConcurrentSkipListMap<Integer, HybridIntMap<Integer>> outerHybridIntMapInnrMap,
			String concurrentMapType, int insertionStartIndex,
			int insertionEndIndex, int coldKeyRangeMax, int numHotKey,
			double hotRatio, int casTries, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		benchmarkType = Util.RANDOM_HOT_COLD;
		this.concurrentMapType = concurrentMapType;
		this.outerConcSkipListMap = outerConcSkipListMap;
		this.outerConcHashMap = outerConcHashMap;
		this.outerPureIntMap = outerPureIntMap;
		this.outerHybridIntMapInnrMap = outerHybridIntMapInnrMap;
		initializeFields(startSignal, doneSignal, insertionStartIndex,
				insertionEndIndex, null, hotRatio, numHotKey, coldKeyRangeMax);
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
					simpleInsertToMapInt();
					break;
				case Util.INT_TO_INNER_MAP:
					simpleInsertToMapInnerMap();
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

	private void simpleInsertToMapInt() {

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
		default:// SkipListMap, ConcurrentMap, SynchronisedMap
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
				mapInt.put(new Integer(i), new Integer(i));
			}
			break;
		}

	}

	private void simpleInsertToMapInnerMap() {

		switch (concurrentMapType) {

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
		case Util.SKIP_LIST_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

				mapInnerMap.put(new Integer(i),
						new ConcurrentSkipListMap<Integer, Integer>());
			}
			break;
		case Util.SYNCHRONIZED_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

				mapInnerMap.put(new Integer(i), Collections
						.synchronizedMap(new HashMap<Integer, Integer>()));
			}
			break;
		case Util.CONCURRENT_MAP:
			for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
				mapInnerMap.put(new Integer(i),
						new ConcurrentSkipListMap<Integer, Integer>());
			}
			break;
		}
	}

	private void randomInsertion_SKIP_LIST_WITH_INNER_CONC_SKIP_LIST_MAP() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentSkipListMap<Integer, Integer> newMap = new ConcurrentSkipListMap<Integer, Integer>();
			ConcurrentSkipListMap<Integer, Integer> innerMap = outerConcSkipListMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue(i);
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

			Integer innerKey = nextInnerMapKeyValue(i);
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private void randomInsertion_PURE_WITH_INNER_PURE_MAP() {

		Integer randomKey;
		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {
			randomKey = nextHotOrColdKeyOuterMap(i);
			PureIntMap<Integer> newMap = new PureIntMap<Integer>();
			PureIntMap<Integer> innerMap = outerPureIntMap.putIfAbsent(
					randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue(i);
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private void randomInsertion_CONCURRENT_HASH_MAP_WITH_INNER_CONC_HASH_MAP() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			Integer randomKey = nextHotOrColdKeyOuterMap(i);

			ConcurrentHashMap<Integer, Integer> newMap = new ConcurrentHashMap<Integer, Integer>();
			ConcurrentHashMap<Integer, Integer> innerMap = outerConcHashMap
					.putIfAbsent(randomKey, newMap);
			if (innerMap == null) {
				innerMap = newMap;
			}
			Integer innerKey = nextInnerMapKeyValue(i);
			Integer innerValue = innerKey;
			innerMap.put(innerKey, innerValue);
		}
	}

	private Integer nextInnerMapKeyValue(int offlineKey) {
		if (offlineInnerKeys) {
			return new Integer(offlineKey);
		}
		return new Integer(randomGen.nextInt());
	}

	private Integer nextHotOrColdKeyOuterMap(int offlineKey) {

		Integer randomKey;
		double hotOrRandomKey = randomGen.nextDouble();// uniformly distributed
														// double value between
														// 0.0 and 1.0
		if (hotOrRandomKey <= hotRatio) {
			randomKey = Util.nextHotKey(randomGen, coldKeyRangeMax, numHotKey);
		} else {
			if (offlineOuterColdKeys) {
				randomKey = new Integer(offlineKey);
			} else {
				randomKey = new Integer(randomGen.nextInt(coldKeyRangeMax));
			}
		}
		return randomKey;
	}

	private void initializeFields(CountDownLatch startSignal,
			CountDownLatch doneSignal, int insertionStartIndex,
			int insertionEndIndex, String mapValuetype, double hotRatio,
			int numHotKey, int coldKeyRangeMax) {
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		this.mapValueType = mapValuetype;
		this.hotRatio = hotRatio;
		this.numHotKey = numHotKey;
		this.coldKeyRangeMax = coldKeyRangeMax;
	}
}