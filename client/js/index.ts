import Notes from "./utils/notes.js";
import FABs from "./utils/fabs.js";
import NoteRepository from "./NoteRepository.js";
import Units from "./utils/units.js";

FABs.toggle(
    document.getElementById("main-fab") as HTMLDivElement,
    document.getElementById("fab-options") as HTMLDivElement
);

const HOME_ROUTE = "http://localhost:5000/" as const;

const noteRepo = new NoteRepository(HOME_ROUTE);

FABs.withFabContainer(
    document.getElementById("fab-options") as HTMLDivElement,
    FABs.onFABContainerClicked(noteRepo)
);

function onNotesFetched() {
    const noteContainer = document.getElementById(
        "notes"
    ) as HTMLDivElement | null;

    if (noteContainer === null) {
        console.error("Unable to find div with id 'notes'");
        return;
    }

    Notes.withNoteContainer(
        noteContainer,
        Notes.addNotes(noteRepo, Units.dimensionsOf(noteContainer))
    );

    noteContainer.firstElementChild?.scrollIntoView();
    window.scrollTo(0, 0);

    Notes.withNoteContainer(noteContainer, Notes.onNoteClicked(noteRepo));
}

noteRepo.fetchAllNotes().then(onNotesFetched).catch(console.error);
