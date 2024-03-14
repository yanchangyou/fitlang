package my.lang.page.diff;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.diff.DiffManager;
import com.intellij.diff.DiffRequestPanel;
import com.intellij.diff.contents.DiffContent;
import com.intellij.diff.contents.DocumentContentImpl;
import com.intellij.diff.requests.ContentDiffRequest;
import com.intellij.json.json5.Json5FileType;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.impl.DocumentImpl;
import com.intellij.openapi.fileTypes.PlainTextFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsContexts;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.intellij.ui.ComponentUtil.getWindow;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public class JsonDiffResultPanel extends JPanel {

    Project project;

    DiffRequestPanel diffRequestPanel;

    public JsonDiffResultPanel(Project project) {

        super(true);

        this.project = project;
        setLayout(new BorderLayout());

        JComponent component = createCenterPanel();
        add(component, BorderLayout.CENTER);
    }

    protected JComponent createCenterPanel() {

        Disposable disposable = new Disposable() {
            @Override
            public void dispose() {

            }
        };

        diffRequestPanel = DiffManager.getInstance().createRequestPanel(project, disposable, getWindow(this));

        return diffRequestPanel.getComponent();
    }

    public void showDiff(final Object input1, final Object input2, boolean needSort) {

        diffRequestPanel.setRequest(new ContentDiffRequest() {
            @Override
            public @NotNull List<DiffContent> getContents() {

                Object object1 = input1;
                Object object2 = input2;

                boolean isJson1 = false;

                boolean isJson2 = false;

                if (input1 instanceof JSONObject || input1 instanceof JSONArray) {
                    isJson1 = true;
                    if (needSort) {
                        if (input1 instanceof JSONArray) {
                            object1 = ExecuteJsonNodeUtil.sortJsonField((JSONArray) input1);
                        } else {
                            object1 = ExecuteJsonNodeUtil.sortJsonField((JSONObject) input1);
                        }
                    }
                    if (input1 instanceof JSONArray) {
                        object1 = toJsonTextWithFormat((JSONArray) object1);
                    } else {
                        object1 = toJsonTextWithFormat((JSONObject) object1);
                    }
                } else if (needSort) {
                    String text = object1.toString();
                    String[] lines = text.split("\r\n|\r|\n");
                    Arrays.sort(lines);
                    object1 = StrUtil.join("\n", (Object) lines);
                }

                if (input2 instanceof JSONObject || input2 instanceof JSONArray) {
                    isJson2 = true;
                    if (needSort) {
                        if (input2 instanceof JSONArray) {
                            object2 = ExecuteJsonNodeUtil.sortJsonField((JSONArray) input2);
                        } else {
                            object2 = ExecuteJsonNodeUtil.sortJsonField((JSONObject) input2);
                        }
                    }
                    if (input2 instanceof JSONArray) {
                        object2 = toJsonTextWithFormat((JSONArray) object2);
                    } else {
                        object2 = toJsonTextWithFormat((JSONObject) object2);
                    }
                } else if (needSort) {
                    String text = object2.toString();
                    String[] lines = text.split("\r\n|\r|\n");
                    Arrays.sort(lines);
                    object2 = StrUtil.join("\n", (Object) lines);
                }

                Document document1 = new DocumentImpl(object1.toString());
                Document document2 = new DocumentImpl(object2.toString());

                document1.setReadOnly(true);
                document2.setReadOnly(true);

                DocumentContentImpl diffContent1;
                if (isJson1) {
                    diffContent1 = new DocumentContentImpl(project, document1, Json5FileType.INSTANCE);
                } else {
                    diffContent1 = new DocumentContentImpl(project, document1, PlainTextFileType.INSTANCE);
                }

                DocumentContentImpl diffContent2;
                if (isJson2) {
                    diffContent2 = new DocumentContentImpl(project, document2, Json5FileType.INSTANCE);
                } else {
                    diffContent2 = new DocumentContentImpl(project, document2, PlainTextFileType.INSTANCE);
                }

                List<DiffContent> list = new ArrayList<>();
                list.add(diffContent1);
                list.add(diffContent2);

                return list;
            }

            @Override
            public @NotNull List<@Nls String> getContentTitles() {
                List<String> titles = new ArrayList<>();
                for (int i = 0; i < 2; i++) {
                    titles.add("");
                }
                return titles;
            }

            @Override
            public @NlsContexts.DialogTitle @Nullable String getTitle() {
                return "Diff";
            }
        });
    }
}
