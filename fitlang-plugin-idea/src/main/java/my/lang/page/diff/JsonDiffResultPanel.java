package my.lang.page.diff;

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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.NlsContexts;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
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

    public void showDiff(JSONObject json1, JSONObject json2) {

        diffRequestPanel.setRequest(new ContentDiffRequest() {
            @Override
            public @NotNull List<DiffContent> getContents() {


                Document document1 = new DocumentImpl(toJsonTextWithFormat(json1));
                Document document2 = new DocumentImpl(toJsonTextWithFormat(json2));

                document1.setReadOnly(false);
                document2.setReadOnly(false);

                DocumentContentImpl diffContent1 = new DocumentContentImpl(project, document1, Json5FileType.INSTANCE);
                DocumentContentImpl diffContent2 = new DocumentContentImpl(project, document2, Json5FileType.INSTANCE);

                List<DiffContent> list = new ArrayList<>();
                list.add(diffContent1);
                list.add(diffContent2);

                return list;
            }

            @Override
            public @NotNull List<@Nls String> getContentTitles() {
                List<String> titles = new ArrayList<>();
                for (int i = 0; i < 2; i++) {
                    titles.add("json" + (i + 1));
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
