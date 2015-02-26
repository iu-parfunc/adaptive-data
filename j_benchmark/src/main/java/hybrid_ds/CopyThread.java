package hybrid_ds;

import java.util.HashSet;
import java.util.Iterator;
import java.util.concurrent.CountDownLatch;

import org.pcollections.IntTreePMap;

public class CopyThread<V> extends Thread {

	private HybridIntMap<V> hybridDS;
	private Iterator<Integer> keySet;

	public CopyThread(HybridIntMap<V> hybridDS, IntTreePMap<V> pureDS,
			Iterator<Integer> keySet) {
		this.hybridDS = hybridDS;
		this.keySet = keySet;
	}

	@Override
	public void run() {

		Integer nextKey;
		while (keySet.hasNext()) {
			nextKey = keySet.next();
			hybridDS.copyFromPureIfAbsent(nextKey, hybridDS.get(nextKey));
		}
		hybridDS.copyIsDone();
		// System.out.println("COPY IS DONE");
	}
}
