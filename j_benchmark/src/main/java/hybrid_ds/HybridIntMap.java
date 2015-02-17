package hybrid_ds;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.atomic.AtomicReference;

import org.pcollections.IntTreePMap;

public class HybridIntMap<V> implements ConcurrentMap<Integer, V> {

	private enum State {
		A, B, AB
	};

	private State state;

	private ConcurrentSkipListMap<Integer, V> outerConcSkipListMap;
	private AtomicReference<IntTreePMap<AtomicReference<V>>> outerMutableIntTreeMap = new AtomicReference(
			IntTreePMap.empty());

	public HybridIntMap() {

		setState(State.A);
	}

	public void contentionDetected(){
		if (!state.equals(state.A)){
			return;
		}
		setState(State.AB);
		/*** TBD ***/
		switchMaps();
	}
	
	private void switchMaps(){
		/*** TBD ***/
		setState(State.B);
	}
	@Override
	public int size() {
		switch (state) {
		case A:
			break;
		case B:
			break;
		case AB:
			break;
		default:
			break;
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
	public V get(Object key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public V put(Integer key, V value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public V remove(Object key) {
		// TODO Auto-generated method stub
		return null;
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

	@Override
	public V putIfAbsent(Integer key, V value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean remove(Object key, Object value) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean replace(Integer key, V oldValue, V newValue) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public V replace(Integer key, V value) {
		// TODO Auto-generated method stub
		return null;
	}

	private State getState() {
		return state;
	}

	private void setState(State state) {
		this.state = state;
	}
}
