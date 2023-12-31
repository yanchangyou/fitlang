// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.MultiMap;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author Irina.Chernushina on 4/22/2017.
 */
public class MatchResult {
  public final List<JsonSchemaObject> mySchemas;
  public final List<Collection<? extends JsonSchemaObject>> myExcludingSchemas;

  private MatchResult(@NotNull final List<JsonSchemaObject> schemas, @NotNull final List<Collection<? extends JsonSchemaObject>> excludingSchemas) {
    mySchemas = Collections.unmodifiableList(schemas);
    myExcludingSchemas = Collections.unmodifiableList(excludingSchemas);
  }

  public static MatchResult create(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode root) {
    List<JsonSchemaObject> schemas = new ArrayList<>();
    MultiMap<Integer, JsonSchemaObject> oneOfGroups = MultiMap.create();
    iterateTree(root, node -> {
      if (node.isAny()) return true;
      int groupNumber = node.getExcludingGroupNumber();
      if (groupNumber < 0) {
        schemas.add(node.getSchema());
      }
      else {
        oneOfGroups.putValue(groupNumber, node.getSchema());
      }
      return true;
    });
    List<Collection<? extends JsonSchemaObject>> result = oneOfGroups.isEmpty()
                                                          ? ContainerUtil.emptyList()
                                                          : new ArrayList<>(oneOfGroups.keySet().size());
    for (Map.Entry<Integer, Collection<JsonSchemaObject>> entry: oneOfGroups.entrySet()) {
      result.add(entry.getValue());
    }
    return new MatchResult(schemas, result);
  }

  public static void iterateTree(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode root,
                                 @NotNull final Processor<? super fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode> processor) {
    final ArrayDeque<fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode> queue = new ArrayDeque<>(root.getChildren());
    while (!queue.isEmpty()) {
      final JsonSchemaTreeNode node = queue.removeFirst();
      if (node.getChildren().isEmpty()) {
        if (!node.isNothing() && fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal.equals(node.getResolveState()) && !processor.process(node)) {
          break;
        }
      } else {
        queue.addAll(node.getChildren());
      }
    }
  }
}
