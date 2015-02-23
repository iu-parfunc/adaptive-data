package hybrid_ds;

import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class PureIntMap<V> {

	private AtomicReference<IntTreePMap<V>> mutableIntTreeMap;

	public PureIntMap() {
		mutableIntTreeMap = new AtomicReference<IntTreePMap<V>>(
				IntTreePMap.empty());
	}

	public V put(Integer key, V value) {

		IntTreePMap<V> snapShot = mutableIntTreeMap.get();
		while (!mutableIntTreeMap.compareAndSet(snapShot,
				snapShot.plus(key, value))) {
			snapShot = mutableIntTreeMap.get();
		}
		return snapShot.get(key);
	}

	public V get(Integer key) {
		return mutableIntTreeMap.get().get(key);
	}

	public V putIfAbsent(Integer key, V value) {
		IntTreePMap<V> snapShot = mutableIntTreeMap.get();
		if (snapShot.containsKey(key)) {
			return snapShot.get(key);
		}
		while (!mutableIntTreeMap.compareAndSet(snapShot,
				snapShot.plus(key, value))) {
			snapShot = mutableIntTreeMap.get();
			if (snapShot.containsKey(key)) {
				return snapShot.get(key);
			}
		}
		return snapShot.get(key);
	}

	public V remove(Integer key) {
		IntTreePMap<V> snapShot = mutableIntTreeMap.get();
		while (!mutableIntTreeMap.compareAndSet(snapShot, snapShot.minus(key))) {
			snapShot = mutableIntTreeMap.get();
		}
		return snapShot.get(key);
	}

	public int size() {
		return mutableIntTreeMap.get().size();
	}

	@Override
	public String toString() {
		return mutableIntTreeMap.get().toString();
	}
}
