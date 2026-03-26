---
name: text-hopper
description: Method to extract words using the hopper method
---

# Intro

The hopper method is a way to extract words from piece of text using a few
simple, custom rules.

# Algorithm

1. The first word to return is the first verb in the text
2. The next word is the object that goes with the verb. If there is none, return
   the subject. If that is not present either, continue to step 3
3. Find the next verb in the text, then go back to step 2. If there are no more
   verbs, end.

# Result

The result of a text hop is a structured string formatted like this

```
verb(object);verb[subject];verb;verb[subject]
```

Example

```
ate(apple);tired[James];worry;yell[sara]
```

# Text to hop through

When skills are available to the tool, the platform adds each skill’s name,
description, and path to user prompt context so the model knows the skill
exists.

The model decides whether to invoke a skill based on this metadata. If the model
invokes a skill, it uses the path to read the full Markdown instructions from
SKILL.md.

Skill instructions are user prompt input (not system prompt input), so they’re
handled with the same priority as other user-provided instructions. For explicit
control, you can still instruct the model to “use the <skill name> skill.”

are[skills];adds(each skill’s name, description, and path);knows(the skill
exists);exists[skill];decides(whether to invoke a skill);invoke(a
skill);invokes(a skill);uses(the path);read(the full Markdown
instructions);are[Skill instructions];handled[they];instruct(the model);use(the
<skill name> skill)
