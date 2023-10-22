package my.lang.completion;

import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionProvider;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import fit.intellij.json.codeinsight.JsonCompletionContributor;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonStringLiteral;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

/**
 * @author yanchangyou
 */
public class MyLanguageCompletionContributor extends JsonCompletionContributor {

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COLON_IN_PROPERTY = psiElement()
            .afterLeaf(":").withSuperParent(2, JsonProperty.class)
            .andNot(psiElement().withParent(JsonStringLiteral.class));

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COMMA_OR_BRACKET_IN_ARRAY = psiElement()
            .afterLeaf("[", ",").withSuperParent(2, JsonArray.class)
            .andNot(psiElement().withParent(JsonStringLiteral.class));

    private static final PsiElementPattern.Capture<PsiElement> TEST = psiElement()
            .afterLeaf(":").withSuperParent(2, JsonProperty.class)
            .andNot(psiElement().withParent(JsonStringLiteral.class));

    public MyLanguageCompletionContributor() {
        extend(CompletionType.BASIC, AFTER_COLON_IN_PROPERTY, MyLanguageCompletionContributor.MyKeywordsCompletionProvider.INSTANCE);
        extend(CompletionType.BASIC, AFTER_COMMA_OR_BRACKET_IN_ARRAY, MyLanguageCompletionContributor.MyKeywordsCompletionProvider.INSTANCE);
    }

    private static class MyKeywordsCompletionProvider extends CompletionProvider<CompletionParameters> {
        private static final MyLanguageCompletionContributor.MyKeywordsCompletionProvider INSTANCE = new MyLanguageCompletionContributor.MyKeywordsCompletionProvider();
        private static final String[] KEYWORDS = new String[]{"null", "true", "false", "uni", "hello"};

        @Override
        protected void addCompletions(@NotNull CompletionParameters parameters,
                                      @NotNull ProcessingContext context,
                                      @NotNull CompletionResultSet result) {
            for (String keyword : KEYWORDS) {
                if (keyword.equals("hello")) {
                    result.addElement(LookupElementBuilder.create("\"" + keyword + "\"").bold());
                } else {
                    result.addElement(LookupElementBuilder.create(keyword).bold());
                }
            }
        }
    }
}

