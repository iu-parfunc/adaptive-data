package benchmark;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

public class Util {

	public static final String SYNCHRONIZED_MAP = "SYNCHRONIZED_HASH_MAP";
	public static final String CONCURRENT_MAP = "CONCURRENT_HASH_MAP";
	public static final String SKIP_LIST_MAP = "SKIP_LIST_MAP";
	public static final String MUTABLE_INT_TREE_MAP = "MUTABLE_INT_TREE_MAP";
	public static final String INT_TO_INT = "INT_TO_INT";
	public static final String INT_TO_MUTABLE_INT_TREE_MAP = "INT_TO_MUTABLE_INT_TREE_MAP";
	public static final String INT_TO_SYNCH_HASHMAP_INT_TO_INT = "INT_TO_SYNCH_HASHMAP_INT_TO_INT";
	public static final String RANDOM_HO_COLD_WITH_INSERTION_TO_INNETMAP = "RANDOM_HO_COLD_WITH_INSERTION_TO_INNETMAP";
	public static final String SIMPLE_INSERTION_BENCHMARK = "SIMEPLE_INSERTION_BENCHMARK";
	public static final String INSERT_TO_MUTABLE_INT_TREE_MAP = "INSERT_TO_MUTABLE_INT_TREE_MAP";

	public static void writeLine(BufferedWriter writer, String line)
			throws IOException {

		writer.write(line);
		writer.newLine();
	}

	public static Integer getNextHotKey(Random hotKeyGen,
			int hotKeyRangeStartsAt, int hotKeyRangeEndsAt,
			double hotKeyPercentage) {

		int hotKeyRangeLength = (hotKeyRangeEndsAt - hotKeyRangeStartsAt);
		int numberOfHotKeys = (int) (((double) hotKeyRangeLength / (double) 100) * hotKeyPercentage);
		return new Integer(
				(int) (hotKeyRangeStartsAt + hotKeyGen.nextInt(numberOfHotKeys)
						* (100 / hotKeyPercentage)));
	}
}
