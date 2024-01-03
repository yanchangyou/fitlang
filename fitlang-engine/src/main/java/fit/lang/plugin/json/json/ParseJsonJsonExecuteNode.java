package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonArrayText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

/**
 * 执行节点
 */
public class ParseJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String jsonField = parseStringField("jsonField", input);
        if (jsonField == null) {
            throw new ExecuteNodeException("parseJson jsonField is required!");
        }
        Object value = input.get(jsonField);

        JSONObject outputJson = input.getData().clone();
        output.setData(outputJson);

        if (value instanceof String) {
            if (isJsonObjectText(value)) {
                value = JSONObject.parse((String) value);
            } else if (isJsonArrayText(value)) {
                value = JSONArray.parse((String) value);
            } else {
                String text = (String) value;
                int begin = text.indexOf("{");
                int end = text.indexOf("}");
                if (begin > -1 && end > begin) {
                    try {
                        value = JSONObject.parse(text.substring(begin, end + 1));
                    } catch (Exception e) {
                        JSONArray array = findJsonArray(text);
                        if (!array.isEmpty()) {
                            value = array;
                        }
                    }
                }
            }

            output.set(jsonField, value);

        }
    }

    /**
     * 尽可能提取文本中的json字符串，返回json数组
     *
     * @param text
     * @return
     */
    static JSONArray findJsonArray(String text) {
        JSONArray array = new JSONArray();

        if (StrUtil.isBlank(text)) {
            return array;
        }
        int begin = text.indexOf('{');
        int end = text.indexOf('}');
        if (begin == -1 || end <= begin) {
            return array;
        }
        int matchFlag = 1;

        int index = begin + 1;

        int timeLimit = 0;//最大循环次数限制，避免死循环

        while (matchFlag != 0 && index < text.length()) {
            timeLimit++;
            if (timeLimit > 100000) {
                break;
            }
            int nextBegin = text.indexOf('{', index);
            int nextEnd = text.indexOf('}', index);
            if (nextEnd == -1 && nextBegin == -1) {
                break;
            }
            if (nextBegin < nextEnd && nextBegin > -1) {
                matchFlag++;
            }
            if (nextEnd > -1) {
                matchFlag--;
            }

            if (matchFlag == 0) {
                String subText = text.substring(begin, nextEnd + 1);
                try {
                    JSONObject jsonObject = JSONObject.parse(subText);
                    array.add(jsonObject);
                    begin = nextBegin;

                    if (nextBegin > -1) {
                        matchFlag = 1;
                    } else {
                        break;
                    }

                } catch (Exception e) {
                    //ignore
                    System.out.println(subText + ":" + e);
                }
            }
            index = nextBegin + 1;
        }

        return array;
    }
}
