export interface NoteSubmission {
    readonly title: string;
    readonly author: string;
    readonly text: string;
    readonly date: number;
}

export interface Note extends NoteSubmission {
    readonly id: number;
}
