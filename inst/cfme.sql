CREATE TABLE "student" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "learner_anon_id" TEXT UNIQUE NOT NULL,
  "pce_assign" TEXT,
  "society" TEXT,
  "acad_prog" TEXT,
  "acad_prog_trk" TEXT,
  "gender" TEXT,
  "urim_flg" TEXT,
  "age" REAL
);

CREATE TABLE "reviewer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "evaluator_id" INTEGER NOT NULL,
  "evaluator" TEXT NOT NULL,
  "acad_title" TEXT
);

CREATE TABLE "clerkship" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "clerkship" TEXT NOT NULL,
  "location" TEXT
);

CREATE TABLE "rotation" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "student_id" INTEGER NOT NULL,
  "clerkship_id" INTEGER NOT NULL,
  "rotation_date" TEXT NOT NULL,
  "first_nbme_score" REAL,
  FOREIGN KEY ("student_id") REFERENCES "student"("id") ON DELETE CASCADE,
  FOREIGN KEY ("clerkship_id") REFERENCES "clerkship"("id") ON DELETE CASCADE
);

CREATE TABLE "evaluation" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "rotation_id" INTEGER NOT NULL,
  "reviewer_id" INTEGER NOT NULL,
  "summary_flg" INTEGER NOT NULL,
  "complete" INTEGER,
  "acad_yr" TEXT,
  FOREIGN KEY ("rotation_id") REFERENCES "rotation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("reviewer_id") REFERENCES "reviewer"("id") ON DELETE CASCADE
);

CREATE TABLE "question" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "question" TEXT NOT NULL
);

CREATE TABLE "answer" (
  "id" INTEGER PRIMARY KEY AUTOINCREMENT,
  "question_id" INTEGER NOT NULL,
  "evaluation_id" INTEGER NOT NULL,
  "submission_date" TEXT NOT NULL,
  "answer_txt" TEXT NOT NULL,
  "answer_txt_redacted" TEXT,
  "rowid" INTEGER,
  FOREIGN KEY ("evaluation_id") REFERENCES "evaluation"("id") ON DELETE CASCADE,
  FOREIGN KEY ("question_id") REFERENCES "question"("id") ON DELETE CASCADE
);
