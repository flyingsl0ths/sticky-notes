"use strict";

// ! Change once deployed
const BACKEND = "http://localhost:8080/";

async function fetchHomePageData() {
  const response = await fetch(BACKEND);
  const json = await response.json();
  return json;
}

const onDisplayHomePage = (json) => {
  const welcomeTag = document.createElement("h1");
  welcomeTag.textContent = json.message;
  document.body.append(welcomeTag);
};

fetchHomePageData().then(onDisplayHomePage).catch(console.log);
