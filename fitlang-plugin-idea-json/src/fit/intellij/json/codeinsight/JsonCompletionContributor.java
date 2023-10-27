/*
 * Copyright 2000-2014 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fit.intellij.json.codeinsight;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonStringLiteral;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

/**
 * @author Mikhail Golubev
 */
public class JsonCompletionContributor extends CompletionContributor {

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COLON_IN_PROPERTY = psiElement().afterLeaf(":").withSuperParent(2, JsonProperty.class).andNot(psiElement().withParent(JsonStringLiteral.class));

    private static final PsiElementPattern.Capture<PsiElement> AFTER_COMMA_OR_BRACKET_IN_ARRAY = psiElement().afterLeaf("[", ",").withSuperParent(2, JsonArray.class).andNot(psiElement().withParent(JsonStringLiteral.class));


    private static final PsiElementPattern.Capture<PsiElement> UNI_KEY_WORD = psiElement().afterLeaf("{");


    public JsonCompletionContributor() {
        extend(CompletionType.BASIC, AFTER_COLON_IN_PROPERTY, MyKeywordsCompletionProvider.INSTANCE);
        extend(CompletionType.BASIC, AFTER_COMMA_OR_BRACKET_IN_ARRAY, MyKeywordsCompletionProvider.INSTANCE);
        extend(CompletionType.BASIC, UNI_KEY_WORD, UniKeywordsCompletionProvider.INSTANCE);
    }

    private static class MyKeywordsCompletionProvider extends CompletionProvider<CompletionParameters> {
        private static final MyKeywordsCompletionProvider INSTANCE = new MyKeywordsCompletionProvider();
        private static final String[] KEYWORDS = new String[]{"null", "true", "false"};

        @Override
        protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
            for (String keyword : KEYWORDS) {
                result.addElement(LookupElementBuilder.create(keyword).bold());
            }
        }
    }


    private static class UniKeywordsCompletionProvider extends CompletionProvider<CompletionParameters> {
        private static final UniKeywordsCompletionProvider INSTANCE = new UniKeywordsCompletionProvider();

        final static String keywords[] = {"hello", "increase", "decrease", "echo", "assert", "set", "sleep", "print", "timeCount", "add", "convert", "removeField", "removeEmptyField", "mix", "mixNode", "eval", "parseJson", "stringifyJson", "convertKeyValueList", "sequence", "pipe", "foreach", "loop", "switch", "return", "thread", "execute", "call", "gitPull", "http", "postJson", "postForm", "httpGet", "httpPut", "httpDelete", "server", "proxy", "web", "wsServer", "wsClient", "readExcel", "writeExcel", "readFile", "writeFile", "deleteFile", "systemInfo", "startMonitor", "getMonitorData", "getClientMonitorData", "receiveClientMonitorData", "pushClientMonitorData", "getMonitorClient", "cloudServer", "cloudClient"};

        @Override
        protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
            for (String keyword : keywords) {
                result.addElement(LookupElementBuilder.create("uni\":\"".concat(keyword)).bold());
            }
        }
    }
}
