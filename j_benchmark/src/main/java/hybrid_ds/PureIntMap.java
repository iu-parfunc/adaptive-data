package hybrid_ds;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class PureIntMap<V> implements Map<Integer, V> {

	private AtomicReference<IntTreePMap<V>> mutableIntTreeMap;

	public PureIntMap() {
		mutableIntTreeMap = new AtomicReference<IntTreePMap<V>>(
				IntTreePMap.empty());
	}

	@Override
	public V put(Integer key, V value) {

		IntTreePMap<V> snapShot = mutableIntTreeMap.get();
		while (!mutableIntTreeMap.compareAndSet(
				snapShot = mutableIntTreeMap.get(), snapShot.plus(key, value)))
			;
		return snapShot.get(key);
	}

	@Override
	public V get(Object key) {
		return mutableIntTreeMap.get().get(key);
	}

	@Override
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
//		return put(key, value);
	}

	@Override
	public V remove(Object key) {
		IntTreePMap<V> snapShot = mutableIntTreeMap.get();
		while (!mutableIntTreeMap.compareAndSet(
				snapShot = mutableIntTreeMap.get(), snapShot.minus(key)))
			;
		return snapShot.get(key);
	}

	@Override
	public int size() {
		return mutableIntTreeMap.get().size();
	}

	@Override
	public String toString() {
		return mutableIntTreeMap.get().toString();
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
	public void clear() {
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
