package hybrid_ds;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class HybridIntMap<V> implements Map<Integer, V> {

	private enum State {
		A, B, AB
	};

	AtomicInteger contentionCount = new AtomicInteger(0);

	V tombstone = null;
	private AtomicReference<State> state;

	private ConcurrentSkipListMap<Integer, V> concSkipListMap = new ConcurrentSkipListMap<Integer, V>();
	private AtomicReference<IntTreePMap<V>> mutableIntTreeMap = new AtomicReference<IntTreePMap<V>>(
			IntTreePMap.empty());

	public HybridIntMap() {
		state = new AtomicReference<HybridIntMap.State>(State.A);
	}

	private void contentionDetected() {
		if (!state.compareAndSet(State.A, State.AB)) {
			return;
		}
		initiateTransition();
	}

	private void initiateTransition() {
		// System.out.println("*** Transition initiated ***");
		concSkipListMap = new ConcurrentSkipListMap<Integer, V>();
		CopyThread<V> copyThread = new CopyThread<V>(this,
				mutableIntTreeMap.get(), mutableIntTreeMap.get().keySet()
						.iterator());
		copyThread.start();
	}

	protected void copyIsDone() {
		state.set(State.B);
	}

	@Override
	public V get(Object key) {
		switch (state.get()) {
		case A:
			return mutableIntTreeMap.get().get(key);
		case B:
			return concSkipListMap.get(key);
		case AB:
			if (concSkipListMap.containsKey(key)) {
				return concSkipListMap.get(key);// A real value or
												// tombstone=null which means
												// there is no mapping
			}
			return mutableIntTreeMap.get().get(key);
		}
		return null;
	}

	@Override
	public V put(Integer key, V value) {

		if (key == null || value == null) {
			throw new NullPointerException();
		}
		int retry = 0;
		int retryThreshold = 10;
		switch (state.get()) {
		case A:

			IntTreePMap<V> lastSnapshot;
			boolean CAS = false;
			lastSnapshot = mutableIntTreeMap.get();
			while ((state.get().equals(State.A))
					&& !(CAS = mutableIntTreeMap.compareAndSet(lastSnapshot,
							lastSnapshot.plus(key, value)))) {
				if (++retry > retryThreshold) {
					contentionDetected();
				}
				lastSnapshot = mutableIntTreeMap.get();
			}

			if (CAS) {
				return lastSnapshot.get(key);
			} else {
				return put(key, value);
			}
		case B:
			return concSkipListMap.put(key, value);

		case AB:
			return concSkipListMap.put(key, value);
		}
		return null;
	}

	@Override
	public V putIfAbsent(Integer key, V value) {
		switch (state.get()) {
		case A:

			IntTreePMap<V> snapShot = mutableIntTreeMap.get();
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

			snapShot = mutableIntTreeMap.get();
			if (!concSkipListMap.containsKey(key) && snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
			if (concSkipListMap.containsKey(key)
					&& concSkipListMap.get(key) == null) {
				concSkipListMap.put(key, value);
			} else {
				return concSkipListMap.putIfAbsent(key, value);
			}
		}
		return null;
	}

	protected void putIfAbsentCopyThead(Integer key, V value) {

		if (concSkipListMap.containsKey(key)) {
			return;
		} else {
			concSkipListMap.putIfAbsent(key, value);
		}
	}

	@Override
	public V remove(Object key) {

		int retry = 0;
		int retryThreshold = 10;

		switch (state.get()) {
		case A:

			IntTreePMap<V> lastSnapshot;
			boolean CAS = false;
			lastSnapshot = mutableIntTreeMap.get();
			while ((state.get().equals(State.A))
					&& !(CAS = mutableIntTreeMap.compareAndSet(lastSnapshot,
							lastSnapshot.minus(key)))) {
				if (++retry > retryThreshold) {
					contentionDetected();
				}
				lastSnapshot = mutableIntTreeMap.get();
			}

			if (CAS) {
				return lastSnapshot.get(key);
			} else {
				return remove(key);
			}
		case B:
			return concSkipListMap.remove(key);
		case AB:
			V previous = concSkipListMap.get(key);
			concSkipListMap.put((Integer) key, null);
			return previous;
		}
		return null;
	}

	@Override
	public void clear() {
		mutableIntTreeMap = new AtomicReference<IntTreePMap<V>>(
				IntTreePMap.empty());
		state.set(State.A);
		concSkipListMap.clear();
	}

	@Override
	public String toString() {
		switch (state.get()) {
		case A:
			return mutableIntTreeMap.get().toString();
		case B:
			return concSkipListMap.toString();
		default:
			return "IN TRANSITION STATE!";
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