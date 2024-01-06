package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;

/**
 * 用户IDE接口
 */
public interface UserIdeInterface {
    /**
     * 读取当前编辑器内容
     *
     * @return
     */
    String readEditorContent();

    /**
     * 写入当前内容
     *
     * @param content
     */
    void writeEditorContent(String content);

    /**
     * 读取查询框内容
     */
    String readEditorSearchContent();

    /**
     * 读取替换框内容
     */
    String readEditorReplaceContent();

    void showFirstComponent(String title, JSONObject actionScript, JSONObject context);

}
