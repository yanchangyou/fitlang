package fit.lang.info;

/**
 *
 */
public class NodeExecuteInfo {

    /**
     * 开始时间：毫秒
     */
    long beginTime;

    /**
     * 结束
     */
    long endTime;

    long costTime;

//    int beginCount;
//
//    int endCount;

    public long getBeginTime() {
        return beginTime;
    }

    public void setBeginTime(long beginTime) {
        this.beginTime = beginTime;
    }

    public long getEndTime() {
        return endTime;
    }

    public void setEndTime(long endTime) {
        this.endTime = endTime;
        costTime = endTime - beginTime;
    }

    public long getCostTime() {
        return costTime;
    }

    public void setCostTime(long costTime) {
        this.costTime = costTime;
    }
//
//    public int getBeginCount() {
//        return beginCount;
//    }
//
//    public void increaseBeginCount() {
//        this.beginCount++;
//    }
//
//    public int getEndCount() {
//        return endCount;
//    }
//
//    public void increaseEndCount() {
//        this.endCount++;
//    }

}
