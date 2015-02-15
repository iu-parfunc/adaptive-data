package benchmark;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentHashMap;

public class Inserter extends Thread {

	int insertionStratIndex, insertionEndIndex, numberOfRandomInsertionTries,
			randomColdKeyMax;
	@SuppressWarnings("rawtypes")
	Map map;
	ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerConcSkipListMap;
	ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerConcHashMap;
	String mapValueType, concurrentMapType;
	CountDownLatch startSignal, doneSignal;

	boolean randomInsertionToInnerSLMap = false;
	double hotOrRandomKeyThreshold;

	public Inserter(@SuppressWarnings("rawtypes") Map map,
			int insertionStartIndex, int insertionEndIndex,
			String mapValuetype, CountDownLatch startSignal,
			CountDownLatch doneSignal) {
		this.map = map;
		this.mapValueType = mapValuetype;
		this.insertionStratIndex = insertionStartIndex;
		this.insertionEndIndex = insertionEndIndex;
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
	}

	public Inserter(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> map,
			int randomInsertRepetitions, int randomColdKeyMax,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		this.outerConcSkipListMap = map;
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.numberOfRandomInsertionTries = randomInsertRepetitions;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.randomColdKeyMax = randomColdKeyMax;
		randomInsertionToInnerSLMap = true;
		concurrentMapType = Util.SKIP_LIST_MAP;
	}

	public Inserter(
			ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> map,
			int randomInsertRepetitions, int randomColdKeyMax,
			double hotOrRandomKeyThreshold, CountDownLatch startSignal,
			CountDownLatch doneSignal) {

		this.outerConcHashMap = map;
		this.startSignal = startSignal;
		this.doneSignal = doneSignal;
		this.numberOfRandomInsertionTries = randomInsertRepetitions;
		this.hotOrRandomKeyThreshold = hotOrRandomKeyThreshold;
		this.randomColdKeyMax = randomColdKeyMax;
		randomInsertionToInnerSLMap = true;
		concurrentMapType = Util.CONCURRENT_MAP;

	}

	@SuppressWarnings("unchecked")
	@Override
	public void run() {

		try {

			/* Wait for start signal from the calling thread */
			startSignal.await();
			if (!randomInsertionToInnerSLMap) {
				switch (mapValueType) {
				case Util.INT_TO_INT:

					insertToMapOfNonCollectionTypes();
					break;

				case Util.INT_TO_SYNCH_HASHMAP_INT_TO_INT:
					insertToMapOfMaps();
					break;

				default:
					break;
				}
			} else {
				switch (concurrentMapType) {
				case Util.SKIP_LIST_MAP:

					insertToConcSKipListMapOfConcSKipListMap(randomColdKeyMax);
					break;

				case Util.CONCURRENT_MAP:
					insertToConcHahsMapOfConcHahsMap(randomColdKeyMax);
					break;

				default:
					break;
				}
			}
			/* Signal that this thread is done */
			doneSignal.countDown();

		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private void insertToConcSKipListMapOfConcSKipListMap(int range) {

		Random hotOrRandomKeyGen = new Random();
		Random coldKeyGen = new Random();
		Random hotKeyGen = new Random();

		Integer coldKey, hotKey;
		for (int i = 0; i < numberOfRandomInsertionTries; i++) {

			double hotOrRandomKey = hotOrRandomKeyGen.nextDouble();
			if (hotOrRandomKey < hotOrRandomKeyThreshold) {
				coldKey = new Integer(coldKeyGen.nextInt(range));
				putToOuterAndInnerConcSkipListMap(outerConcSkipListMap,
						coldKey, 1000000);

			} else {
				hotKey = new Integer(Util.HOT_KEYS.get(hotKeyGen
						.nextInt(Util.HOT_KEYS.size())));
				putToOuterAndInnerConcSkipListMap(outerConcSkipListMap, hotKey,
						1000000);
			}
		}
	}

	private void insertToConcHahsMapOfConcHahsMap(int range) {

		Random hotOrRandomKeyGen = new Random();
		Random coldKeyGen = new Random();
		Random hotKeyGen = new Random();

		Integer coldKey, hotKey;
		for (int i = 0; i < numberOfRandomInsertionTries; i++) {

			double hotOrRandomKey = hotOrRandomKeyGen.nextDouble();
			if (hotOrRandomKey < hotOrRandomKeyThreshold) {

				coldKey = new Integer(coldKeyGen.nextInt(range));
				putToOuterAndInnerConcHashMap(outerConcHashMap, coldKey, range);

			} else {
				hotKey = new Integer(Util.HOT_KEYS.get(hotKeyGen
						.nextInt(Util.HOT_KEYS.size())));
				putToOuterAndInnerConcHashMap(outerConcHashMap, hotKey, range);
			}
		}
	}

	private void putToOuterAndInnerConcSkipListMap(
			ConcurrentSkipListMap<Integer, ConcurrentSkipListMap<Integer, Integer>> outerCSLMap,
			Integer key, int range) {
		Random innerMapKeyValGen = new Random();
		ConcurrentSkipListMap<Integer, Integer> newMap = new ConcurrentSkipListMap<Integer, Integer>();
		ConcurrentSkipListMap<Integer, Integer> innerMap = (ConcurrentSkipListMap<Integer, Integer>) outerCSLMap
				.putIfAbsent(key, newMap);
		if (innerMap == null) {
			innerMap = newMap;
		}
		innerMap.put(new Integer(innerMapKeyValGen.nextInt(range)),
				new Integer(innerMapKeyValGen.nextInt(range)));
	}

	private void putToOuterAndInnerConcHashMap(
			ConcurrentHashMap<Integer, ConcurrentHashMap<Integer, Integer>> outerCHMap,
			Integer key, int range) {
		Random innerMapKeyValGen = new Random();
		ConcurrentHashMap<Integer, Integer> newMap = new ConcurrentHashMap<Integer, Integer>();
		ConcurrentHashMap<Integer, Integer> innerMap = (ConcurrentHashMap<Integer, Integer>) outerCHMap
				.putIfAbsent(key, newMap);
		if (innerMap == null) {
			innerMap = newMap;
		}
		innerMap.put(new Integer(innerMapKeyValGen.nextInt(range)),
				new Integer(innerMapKeyValGen.nextInt(range)));
	}

	private void insertToMapOfNonCollectionTypes() {

		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			map.put(new Integer(i), new Integer(i));
		}
	}

	private void insertToMapOfMaps() {
		for (int i = insertionStratIndex; i < insertionEndIndex; i++) {

			map.put(new Integer(i),
					Collections.synchronizedMap(new HashMap<String, String>()));
		}
	}
}
