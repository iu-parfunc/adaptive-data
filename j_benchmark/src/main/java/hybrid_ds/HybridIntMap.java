package hybrid_ds;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

import benchmark.Util;

public class HybridIntMap<V> implements Map<Integer, V> {

	private enum State {
		A, B, AB, ABinit
	};

	private class HyState<V> {
		private State state;
		private IntTreePMap<V> pureMap;

		public HyState(State state, IntTreePMap<V> pureMap) {
			this.state = state;
			this.pureMap = pureMap;
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

	private HyState<V> hyStateModifiedStateCopy(
			HybridIntMap<V>.HyState<V> lastSnapshot, State state) {
		return new HyState<V>(state, lastSnapshot.pureMap);
	}

	private ConcurrentSkipListMap<Integer, V> concSkipListMap;
	private AtomicReference<HyState<V>> hyState;
	private AtomicBoolean transmitted = new AtomicBoolean(false);

	public HybridIntMap() {
		hyState = new AtomicReference<HyState<V>>(new HyState<V>(State.A,
				IntTreePMap.empty()));
	}

	private void contentionDetected(Integer key) {
		HyState<V> lastSnapshot;
		if (transmitted.compareAndSet(false, true)) {
			while (!hyState.compareAndSet(lastSnapshot = hyState.get(),
					hyStateModifiedStateCopy(lastSnapshot, State.ABinit)))
				;
			concSkipListMap = new ConcurrentSkipListMap<Integer, V>();

			while (!hyState.compareAndSet(lastSnapshot = hyState.get(),
					hyStateModifiedStateCopy(lastSnapshot, State.AB)))
				;
			// System.out.println("*** Transition initiated *** " + key);
			initiateTransition();
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
		while (!hyState.compareAndSet(lastSnapshot = hyState.get(),
				hyStateModifiedStateCopy(lastSnapshot, State.B)))
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
			return concSkipListMap.get(key);
		case AB:
			if (concSkipListMap.containsKey(key)) {
				return concSkipListMap.get(key);// A real value or
												// tombstone=null which means
												// there is no mapping
			}
			return lastSnapshot.pureMap.get(key);
		case ABinit:
			while (hyState.get().state.equals(State.ABinit))
				;
			return get(key);
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
			return concSkipListMap.put(key, value);
		case AB:
			return concSkipListMap.put(key, value);
		case ABinit:
			while (hyState.get().state.equals(State.ABinit))
				;
			return tryPut(key, value, 0);
		}
		return null;
	}

	@Override
	public V putIfAbsent(Integer key, V value) {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			IntTreePMap<V> snapShot = lastSnapshot.pureMap;
			if (snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
			return put(key, value);
		case B:
			if (concSkipListMap.containsKey(key)
					&& concSkipListMap.get(key) == null) {
				return concSkipListMap.put(key, value);
			} else {
				return concSkipListMap.putIfAbsent(key, value);
			}
		case AB:
			snapShot = lastSnapshot.pureMap;
			if (!concSkipListMap.containsKey(key) && snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
			if (concSkipListMap.containsKey(key)
					&& concSkipListMap.get(key) == null) {
				concSkipListMap.put(key, value);
			} else {
				return concSkipListMap.putIfAbsent(key, value);
			}
		case ABinit:
			while (hyState.get().state.equals(State.ABinit))
				;
			return putIfAbsent(key, value);
		}
		return null;
	}

	protected void copyFromPureIfAbsent(Integer key, V value) {

		if (concSkipListMap.containsKey(key)) {
			return;
		} else {
			concSkipListMap.putIfAbsent(key, value);
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
			return concSkipListMap.remove(key);
		case AB:
			V previous = concSkipListMap.get(key);
			concSkipListMap.put((Integer) key, null);
			return previous;
		case ABinit:
			while (hyState.get().state.equals(State.ABinit))
				;
			return tryRemove(key, 0);
		}
		return null;
	}

	@Override
	public void clear() {
		hyState = new AtomicReference<HyState<V>>(new HyState<V>(State.A,
				IntTreePMap.empty()));
		concSkipListMap.clear();
	}

	@Override
	public String toString() {
		HyState<V> lastSnapshot = hyState.get();
		switch (lastSnapshot.state) {
		case A:
			return lastSnapshot.pureMap.toString();
		case B:
			return concSkipListMap.toString();
		default:// ABinit and AB
			return lastSnapshot.pureMap.toString() + "\n"
					+ concSkipListMap.toString();
		}
	}

	@Override
	public int size() {
		// TODO Auto-generated method stub
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