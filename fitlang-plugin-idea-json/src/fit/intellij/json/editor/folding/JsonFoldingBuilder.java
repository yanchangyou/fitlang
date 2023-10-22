package fit.intellij.json.editor.folding;

import fit.intellij.json.JsonElementTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.Couple;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import fit.intellij.json.psi.JsonPsiUtil;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Mikhail Golubev
 */
public class JsonFoldingBuilder implements FoldingBuilder, DumbAware {
  @NotNull
  @Override
  public FoldingDescriptor[] buildFoldRegions(@NotNull ASTNode node, @NotNull Document document) {
    final List<FoldingDescriptor> descriptors = new ArrayList<>();
    collectDescriptorsRecursively(node, document, descriptors);
    return descriptors.toArray(FoldingDescriptor.EMPTY);
  }

  private static void collectDescriptorsRecursively(@NotNull ASTNode node,
                                                    @NotNull Document document,
                                                    @NotNull List<FoldingDescriptor> descriptors) {
    final IElementType type = node.getElementType();
    if ((type == JsonElementTypes.OBJECT || type == JsonElementTypes.ARRAY) && spanMultipleLines(node, document)) {
      descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
    }
    else if (type == JsonElementTypes.BLOCK_COMMENT) {
      descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
    }
    else if (type == JsonElementTypes.LINE_COMMENT) {
      final Couple<PsiElement> commentRange = expandLineCommentsRange(node.getPsi());
      final int startOffset = commentRange.getFirst().getTextRange().getStartOffset();
      final int endOffset = commentRange.getSecond().getTextRange().getEndOffset();
      if (document.getLineNumber(startOffset) != document.getLineNumber(endOffset)) {
        descriptors.add(new FoldingDescriptor(node, new TextRange(startOffset, endOffset)));
      }
    }

    for (ASTNode child : node.getChildren(null)) {
      collectDescriptorsRecursively(child, document, descriptors);
    }
  }

  @Nullable
  @Override
  public String getPlaceholderText(@NotNull ASTNode node) {
    final IElementType type = node.getElementType();
    if (type == JsonElementTypes.OBJECT) {
      final fit.intellij.json.psi.JsonObject object = node.getPsi(fit.intellij.json.psi.JsonObject.class);
      final List<fit.intellij.json.psi.JsonProperty> properties = object.getPropertyList();
      fit.intellij.json.psi.JsonProperty candidate = null;
      for (fit.intellij.json.psi.JsonProperty property : properties) {
        final String name = property.getName();
        final JsonValue value = property.getValue();
        if (value instanceof fit.intellij.json.psi.JsonLiteral) {
          if ("id".equals(name) || "name".equals(name)) {
            candidate = property;
            break;
          }
          if (candidate == null) {
            candidate = property;
          }
        }
      }
      if (candidate != null) {
        return "{\"" + candidate.getName() + "\": " + candidate.getValue().getText() + "...}";
      }
      return "{...}";
    }
    else if (type == JsonElementTypes.ARRAY) {
      return "[...]";
    }
    else if (type == JsonElementTypes.LINE_COMMENT) {
      return "//...";
    }
    else if (type == JsonElementTypes.BLOCK_COMMENT) {
      return "/*...*/";
    }
    return "...";
  }

  @Override
  public boolean isCollapsedByDefault(@NotNull ASTNode node) {
    return false;
  }

  @NotNull
  public static Couple<PsiElement> expandLineCommentsRange(@NotNull PsiElement anchor) {
    return Couple.of(fit.intellij.json.psi.JsonPsiUtil.findFurthestSiblingOfSameType(anchor, false), JsonPsiUtil.findFurthestSiblingOfSameType(anchor, true));
  }

  private static boolean spanMultipleLines(@NotNull ASTNode node, @NotNull Document document) {
    final TextRange range = node.getTextRange();
    int endOffset = range.getEndOffset();
    return document.getLineNumber(range.getStartOffset())
           < (endOffset < document.getTextLength() ? document.getLineNumber(endOffset) : document.getLineCount() - 1);
  }
}
