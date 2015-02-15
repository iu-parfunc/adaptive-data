package benchmark;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;

public class Util {

	public static final String SYNCHRONIZED_MAP = "SYNCHRONIZED_HASH_MAP";
	public static final String CONCURRENT_MAP = "CONCURRENT_HASH_MAP";
	public static final String SKIP_LIST_MAP = "SKIP_LIST_MAP";
	public static final String INT_TO_INT = "INT_TO_INT";
	public static final String INT_TO_SYNCH_HASHMAP_INT_TO_INT = "INT_TO_SYNCH_HASHMAP_INT_TO_INT";

	public static void writeLine(BufferedWriter writer, String line)
			throws IOException {

		writer.write(line);
		writer.newLine();
	}

	public static final ArrayList<Integer> HOT_KEYS = new ArrayList<Integer>();

	static {

		for (int i = 0; i < 1000000; i += 10000) {
			HOT_KEYS.add(new Integer(i));
		}
	}
}
