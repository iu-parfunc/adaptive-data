package hybrid_ds;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class HybridIntMap<V> {

	private enum State {
		A, B, AB
	};
	
	
	V tombstone = null;
	private AtomicReference<State> state;

	private ConcurrentSkipListMap<Integer, V> concSkipListMap = new ConcurrentSkipListMap<Integer, V>();
	private AtomicReference<IntTreePMap<V>> mutableIntTreeMap = new AtomicReference(
			IntTreePMap.empty());

	public HybridIntMap() {

		state = new AtomicReference<HybridIntMap.State>(State.A);
	}

	public void contentionDetected() {
		if (!state.compareAndSet(State.A, State.AB)) {
			return;
		}
		switchMaps();
	}

	private void switchMaps() {
		System.out.println("*** switchMaps ***");
		concSkipListMap = new ConcurrentSkipListMap<Integer, V>();
		CopyThread copyThread = new CopyThread<V>(this,
				mutableIntTreeMap.get(), mutableIntTreeMap.get().keySet()
						.iterator());
		copyThread.start();
	}

	protected void copied() {
		state.set(State.B);
	}

	// @Override
	public int size() {
		switch (state.get()) {
		case A:
			return mutableIntTreeMap.get().size();
		case B:
			return concSkipListMap.size();
		case AB:
			// Remove has not been implemented yet
			break;
		}
		return 0;
	}

	public V get(Object key) {
		switch (state.get()) {
		case A:
			return mutableIntTreeMap.get().get(key);
		case B:
			return concSkipListMap.get(key);
		case AB:
			if (concSkipListMap.containsKey(key)) {
				return concSkipListMap.get(key);
			}
			return mutableIntTreeMap.get().get(key);
		}
		return null;
	}

	public V tryPut(Integer key, V value) {

		if (key == null || value == null) {
			throw new NullPointerException();
		}
		switch (state.get()) {
		case A:

			IntTreePMap<V> lastSnapshot;
			boolean CAS = false;
			do {
				lastSnapshot = mutableIntTreeMap.get();
			} while ((state.get().equals(State.A))
					&& !(CAS = mutableIntTreeMap.compareAndSet(lastSnapshot,
							lastSnapshot.plus(key, value))));

			if (state.get().equals(State.A) && CAS) {
				return lastSnapshot.get(key);
			}
			if (!CAS) {
				return concSkipListMap.put(key, value);
			}
		case B:
			return concSkipListMap.put(key, value);

		case AB:
			return concSkipListMap.put(key, value);
		}
		return null;
	}

	protected V putIfAbsent(Integer key, V value) {

		return concSkipListMap.putIfAbsent(key, value);
	}

	public void clear() {

		mutableIntTreeMap = new AtomicReference(IntTreePMap.empty());
		state.getAndSet(State.A);
		concSkipListMap.clear();

	}

	public boolean remove(Object key, Object value) {
		switch (state.get()) {
		case A:
			break;
		case B:
			break;
		case AB:
			break;
		default:
			break;
		}
		return false;
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
}
