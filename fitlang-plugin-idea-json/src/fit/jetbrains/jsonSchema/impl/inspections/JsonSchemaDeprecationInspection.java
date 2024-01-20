// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.intellij.json.psi.JsonElementVisitor;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonValue;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaResolver;
import fit.jetbrains.jsonSchema.impl.MatchResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class JsonSchemaDeprecationInspection extends JsonSchemaBasedInspectionBase {
  @Override
  protected PsiElementVisitor doBuildVisitor(@NotNull JsonValue root,
                                             @Nullable fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                             @NotNull JsonSchemaService service,
                                             @NotNull ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session) {
    if (schema == null) return PsiElementVisitor.EMPTY_VISITOR;
    final fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(root, schema);
    if (walker == null) return PsiElementVisitor.EMPTY_VISITOR;
    Project project = root.getProject();
    return new JsonElementVisitor() {
      @Override
      public void visitProperty(@NotNull fit.intellij.json.psi.JsonProperty o) {
        annotate(o);
        super.visitProperty(o);
      }
      private void annotate(@NotNull JsonProperty o) {
        JsonPointerPosition position = walker.findPosition(o, true);
        if (position == null) return;

        final MatchResult result = new JsonSchemaResolver(project, schema, position).detailedResolve();
        Iterable<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> iterable;
        if (result.myExcludingSchemas.size() == 1) {
          iterable = ContainerUtil.concat(result.mySchemas, result.myExcludingSchemas.get(0));
        } else {
          iterable = result.mySchemas;
        }

        for (JsonSchemaObject object : iterable) {
          String message = object.getDeprecationMessage();
          if (message != null) {
            holder.registerProblem(o.getNameElement(), JsonBundle.message("property.0.is.deprecated.1", o.getName(), message));
            return;
          }
        }
      }
    };
  }
}
