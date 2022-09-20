import type { Note, NoteSubmission } from "./Note.js";
import Optional from "./utils/optional.js";

export enum ResponseCode {
    NOT_FOUND = 404,
    INVALID_REQUEST = 400,
    OK = 200
}

export type NoteSubmissionResult = {
    noteAuthorNameInvalid?: boolean;
    noteTitleNameInvalid?: boolean;
    noteContentInvalid?: boolean;
    status?: string;
};

export default class NoteRepository {
    constructor(endpointRoot: string) {
        if (new.target !== NoteRepository) {
            throw new Error("Subclassing is not allowed");
        }

        this.endPointRoot = endpointRoot;
    }

    async fetchAllNotes(): Promise<Note[]> {
        if (this.notes.size !== 0) {
            this.notes.clear();
        }

        const respsonse = await fetch(`${this.endPointRoot}/notes`);

        const notes: Note[] = await respsonse.json();

        for (const note of notes) {
            this.notes.set(note.id, note);
        }

        return notes;
    }

    async makeSubmission(
        submission: NoteSubmission
    ): Promise<[number, NoteSubmissionResult]> {
        const respsonse = await fetch(`${this.endPointRoot}/note`, {
            method: "POST",
            body: JSON.stringify(submission)
        });

        return [respsonse.status, await respsonse.json()];
    }

    getNote(id: number): Optional<Note> {
        return new Optional(this.notes.get(id));
    }

    getNotes(): IterableIterator<Note> {
        return this.notes.values();
    }

    private notes: Map<number, Note> = new Map();
    private endPointRoot: string;
}
