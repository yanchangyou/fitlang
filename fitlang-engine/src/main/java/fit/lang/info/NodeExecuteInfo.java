package fit.lang.info;

/**
 *
 */
public class NodeExecuteInfo {

    /**
     * 开始时间：毫秒
     */
    long beginTimeMs;

    /**
     * 结束
     */
    long endTimeMs;

    int beginCount;

    int endCount;

    public long getBeginTimeMs() {
        return beginTimeMs;
    }

    public void setBeginTimeMs(long beginTimeMs) {
        this.beginTimeMs = beginTimeMs;
    }

    public long getEndTimeMs() {
        return endTimeMs;
    }

    public void setEndTimeMs(long endTimeMs) {
        this.endTimeMs = endTimeMs;
    }

    public long costTime() {
        return endTimeMs - beginTimeMs;
    }

    public int getBeginCount() {
        return beginCount;
    }

    public void increaseBeginCount() {
        this.beginCount++;
    }

    public int getEndCount() {
        return endCount;
    }

    public void increaseEndCount() {
        this.endCount++;
    }
}
