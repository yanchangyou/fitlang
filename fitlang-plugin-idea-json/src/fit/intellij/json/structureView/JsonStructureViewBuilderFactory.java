package fit.intellij.json.structureView;

import com.intellij.ide.impl.StructureViewWrapperImpl;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.lang.PsiStructureViewFactory;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.extensions.ExtensionPointUtil;
import com.intellij.psi.PsiFile;
import fit.intellij.json.psi.JsonFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author Mikhail Golubev
 */
public class JsonStructureViewBuilderFactory implements PsiStructureViewFactory {

    public JsonStructureViewBuilderFactory() {
//    JsonCustomStructureViewFactory.EP_NAME.addExtensionPointListener(
//      () -> ApplicationManager.getApplication().getMessageBus().syncPublisher(StructureViewWrapperImpl.STRUCTURE_CHANGED).run(),
//      ExtensionPointUtil.createKeyedExtensionDisposable(this, PsiStructureViewFactory.EP_NAME.getPoint(null)));
    }

    @Nullable
    @Override
    public StructureViewBuilder getStructureViewBuilder(@NotNull final PsiFile psiFile) {
        if (!(psiFile instanceof fit.intellij.json.psi.JsonFile)) {
            return null;
        }
        //TODO
        if (true) {
            return null;
        }

        List<JsonCustomStructureViewFactory> extensionList = JsonCustomStructureViewFactory.EP_NAME.getExtensionList();
        if (extensionList.size() > 1) {
            Logger.getInstance(JsonStructureViewBuilderFactory.class)
                    .warn("Several extensions are registered for JsonCustomStructureViewFactory extension point. " +
                            "Conflicts can arise if there are several builders corresponding to the same file.");
        }

        for (JsonCustomStructureViewFactory extension : extensionList) {
            final StructureViewBuilder builder = extension.getStructureViewBuilder((JsonFile) psiFile);
            if (builder != null) {
                return builder;
            }
        }

        return new TreeBasedStructureViewBuilder() {
            @NotNull
            @Override
            public StructureViewModel createStructureViewModel(@Nullable Editor editor) {
                return new JsonStructureViewModel(psiFile, editor);
            }
        };
    }
}
