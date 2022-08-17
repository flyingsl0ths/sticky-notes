import Notes from "./utils/notes.js";
import FABs from "./utils/fabs.js";
import NoteRepository from "./NoteRepository.js";
import Units from "./utils/units.js";
import Components from "./utils/components.js";

Components.toggle(
    document.getElementById("main-fab"),
    document.getElementById("fab-options")
);

const HOME_ROUTE = "http://localhost:5000/" as const;

const noteRepo = new NoteRepository(HOME_ROUTE);

FABs.withFabContainer(
    document.getElementById("fab-options"),
    FABs.onFABContainerClicked
);

function onNotesFetched() {
    const noteContainer = document.getElementById("notes");

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
