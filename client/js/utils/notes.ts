import Components from "./components.js";
import { default as Units, type Dimensions } from "./units.js";
import type NotesRepository from "../NoteRepository.js";
import type { Note } from "../Note.js";

export type NotesContainer = HTMLDivElement;
export type ContainerAction = (container: NotesContainer) => void;

function isNotesContainer(elem: HTMLElement | null): elem is NotesContainer {
    return elem !== null && elem.id === "notes";
}

const M = {
    withNoteContainer(
        container: NotesContainer | null,
        f: (container: NotesContainer) => void
    ): void {
        if (!isNotesContainer(container)) {
            console.error("NotesContainer id is invalid");
            return;
        }

        f(container);
    },

    addNotes(
        repo: NotesRepository,
        parentContainerSize: Dimensions
    ): ContainerAction {
        return (container: NotesContainer) => {
            const notes = repo.getNotes();

            const { width, height } = parentContainerSize;

            const offset = 0.12 as const;

            for (const note of notes) {
                const { x, y } = Units.randomPosition(width, height);
                container.appendChild(
                    Components.makeNoteElement(note, {
                        x: x * offset,
                        y: y * offset
                    })
                );
            }
        };
    },

    onNoteClicked(repo: NotesRepository): ContainerAction {
        return (container: NotesContainer) => {
            container.addEventListener("pointerdown", event => {
                let target: HTMLElement | null = event.target as HTMLElement;

                target = target.closest(".small-card");

                if (target === null) {
                    console.error(
                        "Could not find parent div container in card"
                    );
                    return;
                }

                const noteId = target.dataset["noteId"];

                if (!noteId) {
                    console.error("Note contains no data-note-id attribute");
                    return;
                }

                const parsedNoteId = parseInt(noteId);
                const note = repo.getNote(parsedNoteId);

                const buildModal = (note: Note) => {
                    Components.showModal(Components.makeNoteDetailsModal(note));
                };

                note.ifPresentOrElse(buildModal, () =>
                    console.error("Requested Note does not exist")
                );
            });
        };
    }
};

export default M;
