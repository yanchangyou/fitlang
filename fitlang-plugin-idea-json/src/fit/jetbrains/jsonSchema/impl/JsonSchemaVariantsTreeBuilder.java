// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.SmartList;
import com.intellij.util.ThreeState;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static fit.jetbrains.jsonSchema.JsonPointerUtil.isSelfReference;

/**
 * @author Irina.Chernushina on 4/20/2017.
 */
public class JsonSchemaVariantsTreeBuilder {

  public static fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode buildTree(@NotNull Project project,
                                                                           @NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                                                           @NotNull final JsonPointerPosition position,
                                                                           final boolean skipLastExpand) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode root = new fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode(null, schema);
    JsonSchemaService service = JsonSchemaService.Impl.get(project);
    expandChildSchema(root, schema, service);
    // set root's position since this children are just variants of root
    for (fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode treeNode : root.getChildren()) {
      treeNode.setPosition(position);
    }

    final ArrayDeque<fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode> queue = new ArrayDeque<>(root.getChildren());

    while (!queue.isEmpty()) {
      final fit.jetbrains.jsonSchema.impl.JsonSchemaTreeNode node = queue.removeFirst();
      if (node.isAny() || node.isNothing() || node.getPosition().isEmpty() || node.getSchema() == null) continue;
      final JsonPointerPosition step = node.getPosition();
      if (!typeMatches(step.isObject(0), node.getSchema())) {
        node.nothingChild();
        continue;
      }
      final Pair<ThreeState, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> pair = doSingleStep(step, node.getSchema(), true);
      if (ThreeState.NO.equals(pair.getFirst())) node.nothingChild();
      else if (ThreeState.YES.equals(pair.getFirst())) node.anyChild();
      else {
        // process step results
        assert pair.getSecond() != null;
        if (node.getPosition().size() > 1 || !skipLastExpand) expandChildSchema(node, pair.getSecond(), service);
        else node.setChild(pair.getSecond());
      }

      queue.addAll(node.getChildren());
    }

    return root;
  }

  private static boolean typeMatches(final boolean isObject, @NotNull final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType requiredType = isObject ? fit.jetbrains.jsonSchema.impl.JsonSchemaType._object : fit.jetbrains.jsonSchema.impl.JsonSchemaType._array;
    if (schema.getType() != null) {
      return requiredType.equals(schema.getType());
    }
    if (schema.getTypeVariants() != null) {
      for (JsonSchemaType schemaType : schema.getTypeVariants()) {
        if (requiredType.equals(schemaType)) return true;
      }
      return false;
    }
    return true;
  }

  private static void expandChildSchema(@NotNull JsonSchemaTreeNode node,
                                        @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject childSchema,
                                        @NotNull JsonSchemaService service) {
    if (interestingSchema(childSchema)) {
      node.createChildrenFromOperation(getOperation(service, childSchema));
    }
    else {
      node.setChild(childSchema);
    }
  }

  @NotNull
  private static Operation getOperation(@NotNull JsonSchemaService service,
                                        fit.jetbrains.jsonSchema.impl.JsonSchemaObject param) {
    final Operation expand = new ProcessDefinitionsOperation(param, service);
    expand.doMap(new HashSet<>());
    expand.doReduce();
    return expand;
  }

  @NotNull
  public static Pair<ThreeState, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> doSingleStep(@NotNull JsonPointerPosition step,
                                                                                              @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject parent,
                                                                                              boolean processAllBranches) {
    final String name = step.getFirstName();
    if (name != null) {
      return propertyStep(name, parent, processAllBranches);
    } else {
      final int index = step.getFirstIndex();
      assert index >= 0;
      return arrayOrNumericPropertyElementStep(index, parent);
    }
  }

  static abstract class Operation {
    @NotNull final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> myAnyOfGroup = new SmartList<>();
    @NotNull final List<List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject>> myOneOfGroup = new SmartList<>();
    @NotNull protected final List<Operation> myChildOperations;
    @NotNull protected final fit.jetbrains.jsonSchema.impl.JsonSchemaObject mySourceNode;
    protected fit.jetbrains.jsonSchema.impl.SchemaResolveState myState = fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal;

    protected Operation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject sourceNode) {
      mySourceNode = sourceNode;
      myChildOperations = new ArrayList<>();
    }

    protected abstract void map(@NotNull Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited);
    protected abstract void reduce();

    public void doMap(@NotNull final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited) {
      map(visited);
      for (Operation operation : myChildOperations) {
        operation.doMap(visited);
      }
    }

    public void doReduce() {
      if (!fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal.equals(myState)) {
        myChildOperations.clear();
        myAnyOfGroup.clear();
        myOneOfGroup.clear();
        return;
      }

      // lets do that to make the returned object smaller
      myAnyOfGroup.forEach(Operation::clearVariants);
      myOneOfGroup.forEach(list -> list.forEach(Operation::clearVariants));

      for (Operation myChildOperation : myChildOperations) {
        myChildOperation.doReduce();
      }
      reduce();
      myChildOperations.clear();
    }

    private static void clearVariants(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject object) {
      object.setAllOf(null);
      object.setAnyOf(null);
      object.setOneOf(null);
    }

    @Nullable
    protected Operation createExpandOperation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                              @NotNull JsonSchemaService service) {
      Operation forConflict = getOperationForConflict(schema, service);
      if (forConflict != null) return forConflict;
      if (schema.getAnyOf() != null) return new AnyOfOperation(schema, service);
      if (schema.getOneOf() != null) return new OneOfOperation(schema, service);
      if (schema.getAllOf() != null) return new AllOfOperation(schema, service);
      return null;
    }

    @Nullable
    private static Operation getOperationForConflict(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                                     @NotNull JsonSchemaService service) {
      // in case of several incompatible operations, choose the most permissive one
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> anyOf = schema.getAnyOf();
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> oneOf = schema.getOneOf();
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> allOf = schema.getAllOf();
      if (anyOf != null && (oneOf != null || allOf != null)) {
        return new AnyOfOperation(schema, service) {{myState = fit.jetbrains.jsonSchema.impl.SchemaResolveState.conflict;}};
      }
      else if (oneOf != null && allOf != null) {
        return new OneOfOperation(schema, service) {{myState = fit.jetbrains.jsonSchema.impl.SchemaResolveState.conflict;}};
      }
      return null;
    }

    protected static List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> mergeOneOf(Operation op) {
      return op.myOneOfGroup.stream().flatMap(List::stream).collect(Collectors.toList());
    }
  }

  // even if there are no definitions to expand, this object may work as an intermediate node in a tree,
  // connecting oneOf and allOf expansion, for example
  private static class ProcessDefinitionsOperation extends Operation {
    private final JsonSchemaService myService;

    protected ProcessDefinitionsOperation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject sourceNode, JsonSchemaService service) {
      super(sourceNode);
      myService = service;
    }

    @Override
    public void map(@NotNull final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited) {
      fit.jetbrains.jsonSchema.impl.JsonSchemaObject current = mySourceNode;
      while (!StringUtil.isEmptyOrSpaces(current.getRef())) {
        final fit.jetbrains.jsonSchema.impl.JsonSchemaObject definition = current.resolveRefSchema(myService);
        if (definition == null) {
          myState = fit.jetbrains.jsonSchema.impl.SchemaResolveState.brokenDefinition;
          return;
        }
        // this definition was already expanded; do not cycle
        if (!visited.add(definition)) break;
        current = fit.jetbrains.jsonSchema.impl.JsonSchemaObject.merge(current, definition, current);
      }
      final Operation expandOperation = createExpandOperation(current, myService);
      if (expandOperation != null) myChildOperations.add(expandOperation);
      else myAnyOfGroup.add(current);
    }

    @Override
    public void reduce() {
      if (!myChildOperations.isEmpty()) {
        assert myChildOperations.size() == 1;
        final Operation operation = myChildOperations.get(0);
        myAnyOfGroup.addAll(operation.myAnyOfGroup);
        myOneOfGroup.addAll(operation.myOneOfGroup);
      }
    }
  }

  private static class AllOfOperation extends Operation {
    private final JsonSchemaService myService;

    protected AllOfOperation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject sourceNode, JsonSchemaService service) {
      super(sourceNode);
      myService = service;
    }

    @Override
    public void map(@NotNull final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited) {
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> allOf = mySourceNode.getAllOf();
      assert allOf != null;
      myChildOperations.addAll(ContainerUtil.map(allOf, sourceNode -> new ProcessDefinitionsOperation(sourceNode, myService)));
    }

    private static <T> int maxSize(List<List<T>> items) {
      if (items.size() == 0) return 0;
      int maxsize = -1;
      for (List<T> item: items) {
        int size = item.size();
        if (maxsize < size) maxsize = size;
      }
      return maxsize;
    }

    @Override
    public void reduce() {
      myAnyOfGroup.add(mySourceNode);

      for (Operation op : myChildOperations) {
        if (!op.myState.equals(fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal)) continue;

        final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> mergedAny = andGroups(op.myAnyOfGroup, myAnyOfGroup);

        final List<List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject>> mergedExclusive =
          new ArrayList<>(op.myAnyOfGroup.size() * maxSize(myOneOfGroup) +
                          myAnyOfGroup.size() * maxSize(op.myOneOfGroup) +
                          maxSize(myOneOfGroup) * maxSize(op.myOneOfGroup));

        for (List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> objects : myOneOfGroup) {
          mergedExclusive.add(andGroups(op.myAnyOfGroup, objects));
        }
        for (List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> objects : op.myOneOfGroup) {
          mergedExclusive.add(andGroups(objects, myAnyOfGroup));
        }
        for (List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> group : op.myOneOfGroup) {
          for (List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> otherGroup : myOneOfGroup) {
            mergedExclusive.add(andGroups(group, otherGroup));
          }
        }

        myAnyOfGroup.clear();
        myOneOfGroup.clear();
        myAnyOfGroup.addAll(mergedAny);
        myOneOfGroup.addAll(mergedExclusive);
      }
    }
  }

  private static List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> andGroups(@NotNull List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> g1,
                                                                                @NotNull List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> g2) {
    List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> result = new ArrayList<>(g1.size() * g2.size());
    for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject s: g1) {
      result.addAll(andGroup(s, g2));
    }
    return result;
  }

  // here is important, which pointer gets the result: lets make them all different, otherwise two schemas of branches of oneOf would be equal
  private static List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> andGroup(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject object, @NotNull List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> group) {
    List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> list = new ArrayList<>(group.size());
    for (fit.jetbrains.jsonSchema.impl.JsonSchemaObject s: group) {
      fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaObject = fit.jetbrains.jsonSchema.impl.JsonSchemaObject.merge(object, s, s);
      if (schemaObject.isValidByExclusion()) {
        list.add(schemaObject);
      }
    }
    return list;
  }

  private static class OneOfOperation extends Operation {
    private final JsonSchemaService myService;

    protected OneOfOperation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject sourceNode, JsonSchemaService service) {
      super(sourceNode);
      myService = service;
    }

    @Override
    public void map(@NotNull final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited) {
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> oneOf = mySourceNode.getOneOf();
      assert oneOf != null;
      myChildOperations.addAll(ContainerUtil.map(oneOf, sourceNode -> new ProcessDefinitionsOperation(sourceNode, myService)));
    }

    @Override
    public void reduce() {
      final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> oneOf = new SmartList<>();
      for (Operation op : myChildOperations) {
        if (!op.myState.equals(fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal)) continue;
        oneOf.addAll(andGroup(mySourceNode, op.myAnyOfGroup));
        oneOf.addAll(andGroup(mySourceNode, mergeOneOf(op)));
      }
      // here it is not a mistake - all children of this node come to oneOf group
      myOneOfGroup.add(oneOf);
    }
  }

  private static class AnyOfOperation extends Operation {
    private final JsonSchemaService myService;

    protected AnyOfOperation(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject sourceNode, JsonSchemaService service) {
      super(sourceNode);
      myService = service;
    }

    @Override
    public void map(@NotNull final Set<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> visited) {
      List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> anyOf = mySourceNode.getAnyOf();
      assert anyOf != null;
      myChildOperations.addAll(ContainerUtil.map(anyOf, sourceNode -> new ProcessDefinitionsOperation(sourceNode, myService)));
    }

    @Override
    public void reduce() {
      for (Operation op : myChildOperations) {
        if (!op.myState.equals(fit.jetbrains.jsonSchema.impl.SchemaResolveState.normal)) continue;

        myAnyOfGroup.addAll(andGroup(mySourceNode, op.myAnyOfGroup));
        for (List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> group : op.myOneOfGroup) {
          myOneOfGroup.add(andGroup(mySourceNode, group));
        }
      }
    }
  }

  private static boolean interestingSchema(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    return schema.getAnyOf() != null || schema.getOneOf() != null || schema.getAllOf() != null || schema.getRef() != null
           || schema.getIfThenElse() != null;
  }


  @NotNull
  private static Pair<ThreeState, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> propertyStep(@NotNull String name,
                                                                                               @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject parent,
                                                                                               boolean processAllBranches) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaObject child = parent.getProperties().get(name);
    if (child != null) {
      return Pair.create(ThreeState.UNSURE, child);
    }
    final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema = parent.getMatchingPatternPropertySchema(name);
    if (schema != null) {
      return Pair.create(ThreeState.UNSURE, schema);
    }
    if (parent.getAdditionalPropertiesSchema() != null) {
      return Pair.create(ThreeState.UNSURE, parent.getAdditionalPropertiesSchema());
    }

    if (processAllBranches) {
      List<fit.jetbrains.jsonSchema.impl.IfThenElse> ifThenElseList = parent.getIfThenElse();
      if (ifThenElseList != null) {
        for (IfThenElse ifThenElse : ifThenElseList) {
          // resolve inside V7 if-then-else conditionals
          fit.jetbrains.jsonSchema.impl.JsonSchemaObject childObject;

          // NOTE: do not resolve inside 'if' itself - it is just a condition, but not an actual validation!
          // only 'then' and 'else' branches provide actual validation sources, but not the 'if' branch

          fit.jetbrains.jsonSchema.impl.JsonSchemaObject then = ifThenElse.getThen();
          //noinspection Duplicates
          if (then != null) {
            childObject = then.getProperties().get(name);
            if (childObject != null) {
              return Pair.create(ThreeState.UNSURE, childObject);
            }
          }
          fit.jetbrains.jsonSchema.impl.JsonSchemaObject elseBranch = ifThenElse.getElse();
          //noinspection Duplicates
          if (elseBranch != null) {
            childObject = elseBranch.getProperties().get(name);
            if (childObject != null) {
              return Pair.create(ThreeState.UNSURE, childObject);
            }
          }
        }
      }
    }
    if (Boolean.FALSE.equals(parent.getAdditionalPropertiesAllowed())) {
      return Pair.create(ThreeState.NO, null);
    }
    // by default, additional properties are allowed
    return Pair.create(ThreeState.YES, null);
  }

  @NotNull
  private static Pair<ThreeState, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> arrayOrNumericPropertyElementStep(int idx, @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject parent) {
    if (parent.getItemsSchema() != null) {
      return Pair.create(ThreeState.UNSURE, parent.getItemsSchema());
    }
    if (parent.getItemsSchemaList() != null) {
      final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> list = parent.getItemsSchemaList();
      if (idx >= 0 && idx < list.size()) {
        return Pair.create(ThreeState.UNSURE, list.get(idx));
      }
    }
    final String keyAsString = String.valueOf(idx);
    if (parent.getProperties().containsKey(keyAsString)) {
      return Pair.create(ThreeState.UNSURE, parent.getProperties().get(keyAsString));
    }
    final JsonSchemaObject matchingPatternPropertySchema = parent.getMatchingPatternPropertySchema(keyAsString);
    if (matchingPatternPropertySchema != null) {
      return Pair.create(ThreeState.UNSURE, matchingPatternPropertySchema);
    }
    if (parent.getAdditionalItemsSchema() != null) {
      return Pair.create(ThreeState.UNSURE, parent.getAdditionalItemsSchema());
    }
    if (Boolean.FALSE.equals(parent.getAdditionalItemsAllowed())) {
      return Pair.create(ThreeState.NO, null);
    }
    return Pair.create(ThreeState.YES, null);
  }

  public static class SchemaUrlSplitter {
    @Nullable
    private final String mySchemaId;
    @NotNull
    private final String myRelativePath;

    public SchemaUrlSplitter(@NotNull final String ref) {
      if (isSelfReference(ref)) {
        mySchemaId = null;
        myRelativePath = "";
        return;
      }
      if (!ref.startsWith("#/")) {
        int idx = ref.indexOf("#/");
        if (idx == -1) {
          mySchemaId = ref.endsWith("#") ? ref.substring(0, ref.length() - 1) : ref;
          myRelativePath = "";
        } else {
          mySchemaId = ref.substring(0, idx);
          myRelativePath = ref.substring(idx);
        }
      } else {
        mySchemaId = null;
        myRelativePath = ref;
      }
    }

    public boolean isAbsolute() {
      return mySchemaId != null;
    }

    @Nullable
    public String getSchemaId() {
      return mySchemaId;
    }

    @NotNull
    public String getRelativePath() {
      return myRelativePath;
    }
  }
}
