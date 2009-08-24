class BufferedReaderWrapper(r:java.io.BufferedReader) {
    def readLines = new Iterator[String] {
        private var line = readLine
        def hasNext = line != null
        def next = {
            val r = line;
            if(r == null) throw new NoSuchElementException
            line = readLine;
            r
        }
        private def readLine = try {
            r.readLine
        } catch {
            case e:java.io.IOException => null
        }
    }
}
