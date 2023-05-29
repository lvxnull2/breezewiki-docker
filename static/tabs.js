"use strict";

function handleTabber(tabber) {
    let [tabs, contents] = getTabs(tabber);

    for (let i in tabs) {
        let tab = tabs[i];
        let content = contents[i];
        tab.addEventListener("click", function(e) {
            let [currentTab, currentContent] = getCurrentTab(tabber);
            if (currentTab) {
                currentTab.classList.remove("wds-is-current");
            }
            if (currentContent) {
                currentContent.classList.remove("wds-is-current");
            }

            tab.classList.add("wds-is-current");
            content.classList.add("wds-is-current");
            e.preventDefault();
        });
    }
}

for (let tabber of document.body.querySelectorAll(".wds-tabber")) {
    handleTabber(tabber);
}
document.body.classList.remove("bw-tabs-nojs");



function getTabs(tabber) {
    let tabs = [];
    let contents = [];

    for (let i of tabber.querySelector(".wds-tabs__wrapper").querySelectorAll(".wds-tabs__tab")) {
        tabs.push(i);
    }
    for (let i of tabber.children) {
        if (!i.matches(".wds-tab__content")) {
            continue;
        }
        contents.push(i);
    }

    return [tabs, contents];
}

function getCurrentTab(tabber) {
    let tab = null;
    let content = null;

    tab = tabber.querySelector(".wds-tabs__wrapper").querySelector(".wds-tabs__tab.wds-is-current");
    for (let i of tabber.children) {
        if (!i.matches(".wds-tab__content.wds-is-current")) {
            continue;
        }
        content = i;
        break;
    }

    return [tab, content];
}
