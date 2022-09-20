import type { Note, NoteSubmission } from "../Note.js";
import { default as NoteRepository, ResponseCode } from "../NoteRepository.js";
import Optional from "./optional.js";

export type Modal = HTMLDivElement;
type ModalBuilder = () => Modal;

const MODAL_FADE_OUT_DURATION = 500 as const;
const MODAL_FADE_IN_DURATION = 100 as const;

function hasModal(elem: HTMLDivElement): elem is Modal {
    const modal = elem.firstElementChild?.firstElementChild;

    if (modal === null || modal == undefined) {
        return false;
    }

    return modal.classList.contains("modal-close-btn");
}

function makeModal(
    modal: () => Modal,
    onClose: Optional<() => void> = new Optional()
): Modal {
    const backdrop = document.createElement("div");

    backdrop.classList.add("modal-bg", "cvbox", "fade-out");

    const closeButton = document.createElement("span");

    closeButton.classList.add("modal-close-btn");

    closeButton.innerText = "X";

    closeButton.addEventListener("pointerdown", () => {
        backdrop.style.opacity = "0";
        onClose.ifPresent(f => f());
        setTimeout(() => backdrop.remove(), MODAL_FADE_OUT_DURATION);
    });

    const content = modal();

    content.prepend(closeButton);

    backdrop.appendChild(content);

    return backdrop;
}

function hasEmptyFields({ title, text }: NoteSubmission): string | null {
    const emptyFields: string[] = [];

    if (title.length === 0) {
        emptyFields.push("- title");
    }
    if (text.length === 0) {
        emptyFields.push("- post");
    }
    if (emptyFields.length !== 0) {
        return emptyFields.reduce((acc, current) => acc + "\n" + current, "");
    }

    return null;
}

function postFormBuilder(repo: NoteRepository): Modal {
    const TEXT_FIELD_MAX_LENGTH = 35;
    const TEXT_AREA_MAX_LENGTH = 250;

    const container = document.createElement("div");
    container.classList.add("cvbox", "post-form");

    const header = document.createElement("h1");
    header.innerText = "Post";
    header.id = "post-form-title";

    const title = document.createElement("input");
    title.maxLength = TEXT_FIELD_MAX_LENGTH;
    title.placeholder = "Title";
    title.classList.add("form-control");

    const author = document.createElement("input");
    author.maxLength = TEXT_FIELD_MAX_LENGTH;
    author.placeholder = "Author";
    author.classList.add("form-control");

    const body = document.createElement("textarea");
    body.maxLength = TEXT_AREA_MAX_LENGTH;
    body.placeholder = "Type something...";
    body.cols = 30;
    body.rows = 10;
    body.classList.add("form-control");

    const submitBtn = document.createElement("h4");
    submitBtn.innerText = "Submit";
    submitBtn.id = "square-button";
    submitBtn.classList.add("form-control");

    const onPostSubmitted = async () => {
        const submission: NoteSubmission = {
            title: title.value,
            author: author.value,
            text: body.value,
            date: Date.now()
        };

        const emptyFields = hasEmptyFields(submission);
        if (emptyFields !== null) {
            alert("Empty:" + emptyFields);
            return;
        }

        const [statusCode, submissionResult] = await repo.makeSubmission(
            submission
        );

        if (statusCode === ResponseCode.NOT_FOUND) {
            console.error(submissionResult);
        } else if (statusCode === ResponseCode.INVALID_REQUEST) {
            alert("Post cannot contain any bad words or urls.");
        } else if (statusCode == ResponseCode.OK) {
            alert("Post submitted");
            title.value = "";
            author.value = "";
            body.value = "";
        } else {
            console.error("Unknown response.");
        }
    };

    submitBtn.addEventListener("pointerdown", onPostSubmitted);

    container.append(header, title, author, body, submitBtn);

    return container;
}

const M = {
    makeNoteElement(
        { title: noteTitle, text: noteContent, id: noteId }: Note,
        { x, y }: { x: number; y: number }
    ): HTMLDivElement {
        const container = document.createElement("div");
        container.classList.add("cvbox", "small-card");
        container.dataset["noteId"] = `${noteId}`;

        container.style.top = `${x}em`;
        container.style.left = `${y}em`;

        const title = document.createElement("h2");
        title.innerText = noteTitle;

        const body = document.createElement("p");
        body.innerText = noteContent;

        container.append(title, body);

        return container;
    },

    makePostModal(repo: NoteRepository): HTMLDivElement {
        return makeModal(() => postFormBuilder(repo));
    },

    makeNoteDetailsModal({
        title: noteTitle,
        text: noteContent,
        date,
        author: noteAuthor
    }: Note): HTMLDivElement {
        const noteDetails: ModalBuilder = () => {
            const container = document.createElement("div");
            container.classList.add("cvbox", "large-card");

            const title = document.createElement("h2");
            title.innerText = noteTitle;

            const body = document.createElement("p");
            body.innerText = noteContent;

            const author = document.createElement("h4");
            author.innerText = `- ${noteAuthor}`;

            const footer = document.createElement("h4");
            footer.innerText = new Date(date).toLocaleString();

            container.append(title, body, author, footer);

            return container;
        };

        return makeModal(noteDetails);
    },

    showModal(modal: HTMLDivElement) {
        if (!hasModal(modal)) {
            console.error("div doest not contain a constructed modal element");
            return;
        }

        // Ensure a double click doesn't happen
        if (!document.body.firstElementChild?.classList.contains("modal-bg")) {
            document.body.prepend(modal);
        }

        setTimeout(
            () => modal.classList.add("modal-active"),
            MODAL_FADE_IN_DURATION
        );
    }
};

export default M;
