"use strict";

// ! CHANGE ONCE DEPLOYED
const HOME_ROUTE = "http://localhost:8080/hello" as const;

type HelloJson = {
    message: string;
};

async function fetchHomePageData(): Promise<HelloJson> {
    const response = await fetch(HOME_ROUTE);
    const json = await response.json();
    return json;
}

const onDisplayHomePage = (json: HelloJson) => {
    const welcomeTag = document.createElement("h1") as HTMLHeadElement;
    welcomeTag.classList.add("header");
    welcomeTag.textContent = json.message;
    document.body.prepend(welcomeTag);
};

fetchHomePageData().then(onDisplayHomePage).catch(console.log);
