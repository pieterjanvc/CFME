# TASK

You previously extracted competency evidence from a clinical clerkship
evaluation, but some of the quotes you returned were assigned to more than one
competency. Each piece of evidence must belong to exactly one competency (the
single most specific match).

Below is a numbered list of conflicts. For each one, decide which single
competency the quote belongs to, or discard it entirely if it does not
clearly belong to any of the listed options.

{disambiguation_section}

# COMPETENCIES

{competencies}

# OUTPUT

Return valid JSON that can be parsed directly, so no markdown, no explanation.
Use this exact structure:

{"resolutions": [{"conflictId": 1, "cIndex": 2}]}

Return exactly one resolution per conflictId listed below. Use the cIndex of
the option you are keeping, or cIndex 0 to discard the quote entirely (assign
it to none of the listed options).
