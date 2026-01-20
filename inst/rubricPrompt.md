# TASK

You will be assessing the quality of a provided a clinical clerkship evaluation
written by a supervisor Detect any of the listed competencies in the provided
evaluation using the rubric below

# RUBRIC

## Competencies

### 1. Medical knowledge

Demonstrate understanding of foundational principles that underlie the medical
sciences and apply this knowledge in care of individuals and populations.
Generate an appropriate differential diagnosis.

### 2. Medical history taking and physical examination

Elicit and synthesize a complete and accurate medical history and perform a
focused or comprehensive physical examination, using information from the
patient and other relevant sources.

### 3. Provide effective oral and written professional communication

Communicate clinical information effectively, efficiently, and professionally in
oral and written formats, including concise patient presentations on rounds and
well-organized clinical documentation such as initial histories and physicals
and daily progress notes to support patient care.

### 4. Clinical reasoning and decision making

Efficiently evaluate patient data and use clinical problem solving to prioritize
a differential diagnosis and establish an assessment and plan.

### 5. Interpersonal and Communication Skills

Form collaborative and trusting relationships with patients, caregivers, staff
and all. Effectively communicate with patients and caregivers to promote shared
decision making.

### 6. Scholarly Inquiry and Evidence-Based Medicine Integration

Evaluate, analyze, and apply new and existing knowledge across biomedical,
clinical, population, and data sciences through continuous self-directed
learning and scholarly activity to advance patient care.

### 7. Professionalism

Exemplify compassion, integrity, social responsibility and respect for all
persons and identities. Demonstrate responsible behaviors including
accountability, patient confidentiality and safety, punctuality and the
prioritizing of the needs of others while maintaining appropriate self-care.
Demonstrate and embody ethical standards, principles and moral reasoning in all
professional interactions with patients, caregivers, colleagues and society at
large. Apply the skills and incorporate the attitudes needed to maintain and
promote personal wellbeing while ensuring patient safety.

### 8. Interprofessional and team-based care

Collaborate effectively within interprofessional healthcare teams by
communicating clearly and respectfully with physicians, nurses, staff and other
health professionals to provide coordinated, patient-centered care.

## compScores: Individual competency scores

Evaluate the quality of each detected competency description using the metrics below. :

### cID: Competency ID (1-n)

### context: Context Score:

- 1: Decontextualized: Mentioned without specific context and use of generic
  language.
- 2: Specific: One or more specific examples are described in some detail, 
  but the impact of those is not clearly listed
- 3: Impactful: Examples are detailed and show direct and concrete impact on 
  specific, measurable outcomes

### text: Verbatim evidence

Pieces of verbatim text from the assessment supporting the previous scores. Only
use the minimal number of words needed.

## overallScores: Overall scores

These scores are for the evaluation as a whole

### util: Utility score indicating how useful the feedback is to the student:

- 1: low/not useful: Uses 3rd person, minimal specific information, often vague
- 2: moderately useful: Specific to the student but hard to act upon
- 3: highly useful: Very specific and directly applicable to professional
  improvement

### sent: Evaluator sentiment score for the overall review

- 1: clearly negative or red flags
- 2: slightly negative or coded language indicating potential criticism
- 3: neutral or not enough information to indicate sentiment
- 4: generic positive language (e.g. aspecific superlatives)
- 5: specific positive language indicating the reviewer's effort to make the
  student stand out

# TO RETURN

A valid JSON string without any other code markup using the template below.
Replace any example values with the real ones! {"compScores": [{"cID": 1,"context":
1,"text": ["some text evidence","other text evidence"]},{}],"util": 1,"sent": 1}
