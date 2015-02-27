package hybrid_ds;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

import benchmark.Util;

public class HybridIntMap<V> implements Map<Integer, V> {

	private enum State {
		A, B, AB
	};

	private class HyState<V> {
		private State state;
		private ConcurrentSkipListMap<Integer, V> concSkipListMap;
		private IntTreePMap<V> pureMap;

		public HyState(State state, IntTreePMap<V> pureMap) {
			this.state = state;
			this.pureMap = pureMap;
		}

		public HyState(State state, IntTreePMap<V> pureMap,
				ConcurrentSkipListMap<Integer, V> concSkipListMap) {
			this.state = state;
			this.pureMap = pureMap;
			this.concSkipListMap = concSkipListMap;
		}
	}

	private HyState<V> hyStatePlus(State state,
			HybridIntMap<V>.HyState<V> lastSnapshot, Integer key, V value) {
		return new HyState<V>(state, lastSnapshot.pureMap.plus(key, value));
	}

	private HyState<V> hyStateMinus(State state,
			HybridIntMap<V>.HyState<V> lastSnapshot, Object key) {
		return new HyState<V>(state, lastSnapshot.pureMap.minus(key));
	}

	private HyState<V> hyStateModifiedStateCopy(State state,
			IntTreePMap<V> pureMap,
			ConcurrentSkipListMap<Integer, V> concSkipListMap) {
		return new HyState<V>(state, pureMap, concSkipListMap);
	}

	private AtomicReference<HyState<V>> hyState;

	public HybridIntMap() {
		hyState = new AtomicReference<HyState<V>>(new HyState<V>(State.A,
				IntTreePMap.empty()));
	}

	private void contentionDetected(Integer key) {
		HyState<V> lastSnapshot;

		while ((lastSnapshot = hyState.get()).state.equals(State.A)) {

			if (hyState.compareAndSet(lastSnapshot, new HyState<V>(State.AB,
					lastSnapshot.pureMap,
					new ConcurrentSkipListMap<Integer, V>()))) {

				// System.out.print("*****");
				initiateTransition();
				break;
			}
		}
	}

	private void initiateTransition() {
		CopyThread<V> copyThread = new CopyThread<V>(this,
				hyState.get().pureMap, hyState.get().pureMap.keySet()
						.iterator());
		copyThread.start();
	}

	protected void copyIsDone() {
		HyState<V> lastSnapshot;
		while (!hyState.compareAndSet(
				lastSnapshot = hyState.get(),
				hyStateModifiedStateCopy(State.B, lastSnapshot.pureMap,
						lastSnapshot.concSkipListMap)))
			;
		;
		hyState.get().pureMap = null;
	}

	@Override
	public V get(Object key) {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			return lastSnapshot.pureMap.get(key);
		case B:
			return lastSnapshot.concSkipListMap.get(key);
		case AB:
			if (lastSnapshot.concSkipListMap.containsKey(key)) {
				return lastSnapshot.concSkipListMap.get(key);// A real value or
				// tombstone=null which means
				// there is no mapping
			}
			return lastSnapshot.pureMap.get(key);
		}
		return null;
	}

	@Override
	public V put(Integer key, V value) {

		if (key == null || value == null) {
			throw new NullPointerException();
		}
		int tries = 0;
		return tryPut(key, value, tries);
	}

	private V tryPut(Integer key, V value, int tries) {

		if (tries > Util.getCasTries()) {
			contentionDetected(key);
			return tryPut(key, value, 0);
		}
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			if (hyState.compareAndSet(lastSnapshot,
					hyStatePlus(lastSnapshot.state, lastSnapshot, key, value))) {
				return lastSnapshot.pureMap.get(key);
			}
			return tryPut(key, value, ++tries);
		case B:
			return lastSnapshot.concSkipListMap.put(key, value);
		case AB:
			return lastSnapshot.concSkipListMap.put(key, value);
		}
		return null;
	}

	@Override
	public V putIfAbsent(Integer key, V value) {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A: {
			IntTreePMap<V> snapShot = lastSnapshot.pureMap;
			if (snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
			return put(key, value);
		}
		case B: {
			if (lastSnapshot.concSkipListMap.containsKey(key)
					&& lastSnapshot.concSkipListMap.get(key) == null) {
				return lastSnapshot.concSkipListMap.put(key, value);
			} else {
				return lastSnapshot.concSkipListMap.putIfAbsent(key, value);
			}
		}
		case AB: {
			IntTreePMap<V> snapShot = lastSnapshot.pureMap;
			if (!lastSnapshot.concSkipListMap.containsKey(key)
					&& snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
			if (lastSnapshot.concSkipListMap.containsKey(key)
					&& lastSnapshot.concSkipListMap.get(key) == null) {
				lastSnapshot.concSkipListMap.put(key, value);
			} else {
				return lastSnapshot.concSkipListMap.putIfAbsent(key, value);
			}
		}
		}
		return null;
	}

	protected void copyFromPureIfAbsent(Integer key, V value) {

		HyState<V> lastSnapshot = hyState.get();
		if (lastSnapshot.concSkipListMap.containsKey(key)) {
			return;
		} else {
			lastSnapshot.concSkipListMap.putIfAbsent(key, value);
		}
	}

	@Override
	public V remove(Object key) {

		int tries = 0;
		return tryRemove(key, tries);

	}

	private V tryRemove(Object key, int tries) {

		if (tries > Util.getCasTries()) {
			contentionDetected((Integer) key);
			return tryRemove(key, 0);
		}
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			if (hyState.compareAndSet(lastSnapshot,
					hyStateMinus(lastSnapshot.state, lastSnapshot, key))) {
				return lastSnapshot.pureMap.get(key);
			}
			return tryRemove(key, ++tries);
		case B:
			return lastSnapshot.concSkipListMap.remove(key);
		case AB:
			V previous = lastSnapshot.concSkipListMap.get(key);
			lastSnapshot.concSkipListMap.put((Integer) key, null);
			return previous;
		}
		return null;
	}

	@Override
	public void clear() {
		hyState = new AtomicReference<HyState<V>>(new HyState<V>(State.A,
				IntTreePMap.empty()));
	}

	@Override
	public String toString() {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			return "A >>> " + lastSnapshot.pureMap.toString();
		case B:
			return "B >>> " + lastSnapshot.concSkipListMap.toString();
		case AB:
			return "AB >>> " + lastSnapshot.pureMap.toString() + "\n"
					+ lastSnapshot.concSkipListMap.toString();
		}
		return null;
	}

	@Override
	public int size() {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			return lastSnapshot.pureMap.size();
		case B:
			return lastSnapshot.concSkipListMap.size();
		case AB:
			HashSet<Integer> union = new HashSet<Integer>();
			union.addAll(hyState.get().pureMap.keySet());
			union.addAll(lastSnapshot.concSkipListMap.keySet());
			return union.size();
		}
		return 0;
	}

	@Override
	public boolean isEmpty() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean containsKey(Object key) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean containsValue(Object value) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void putAll(Map<? extends Integer, ? extends V> m) {
		// TODO Auto-generated method stub

	}

	@Override
	public Set<Integer> keySet() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<V> values() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<java.util.Map.Entry<Integer, V>> entrySet() {
		// TODO Auto-generated method stub
		return null;
	}
}