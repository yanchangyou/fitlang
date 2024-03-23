package my.lang.page.pick;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;

public class PickConfig {

    JSONArray urls;

    Integer rows = 2;
    Integer columns = 2;

    JSONObject selectorConfig;

    JSONArray checkFields;

    Double second = 1.0;

    Integer poolSize;

    public JSONArray getUrls() {
        return urls;
    }

    public void setUrls(JSONArray urls) {
        this.urls = urls;
    }

    public Integer getRows() {
        if (rows == null) {
            rows = 2;
        }
        return rows;
    }

    public void setRows(Integer rows) {
        this.rows = rows;
    }

    public Integer getColumns() {
        if (columns == null) {
            columns = 2;
        }
        return columns;
    }

    public void setColumns(Integer columns) {
        this.columns = columns;
    }

    public JSONObject getSelectorConfig() {
        if (selectorConfig == null) {
            selectorConfig = new JSONObject();
        }
        return selectorConfig;
    }

    public void setSelectorConfig(JSONObject selectorConfig) {
        this.selectorConfig = selectorConfig;
    }

    public JSONArray getCheckFields() {
        if (checkFields == null) {
            checkFields = new JSONArray(0);
        }
        return checkFields;
    }

    public void setCheckFields(JSONArray checkFields) {
        this.checkFields = checkFields;
    }

    public void setSecond(Double second) {
        this.second = second;
    }

    public int getGridTotal() {
        return rows * columns;
    }

    public Integer getPoolSize() {
        if (poolSize == null) {
            poolSize = 1000;
        }
        return poolSize;
    }

    public Double getSecond() {
        if (second == null) {
            second = 1.0;
        }
        return second;
    }

    public void setSecond(double second) {
        this.second = second;
    }

    public void setPoolSize(Integer poolSize) {
        this.poolSize = poolSize;
    }

    public static PickConfig parse(JSONObject config) {
        return config.to(PickConfig.class);
    }
}
