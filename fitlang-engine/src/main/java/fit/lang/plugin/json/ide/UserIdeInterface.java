package fit.lang.plugin.json.ide;

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

}
