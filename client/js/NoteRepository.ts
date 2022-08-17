import type { Note } from "./Note.js";
import Optional from "./utils/optional.js";

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

    getNote(id: number): Optional<Note> {
        return new Optional(this.notes.get(id));
    }

    getNotes(): IterableIterator<Note> {
        return this.notes.values();
    }

    private notes: Map<number, Note> = new Map();
    private endPointRoot: string;
}
