var obj = {
    tagName: 'H1',
    children: [
        {
            tagName: 'SPAN',
            children: [
                'TestString is AWESOME'
            ]
        }
};

function createElement(obj) {
    const element = document.createElement(obj.tagName);
    obj.children.forEach(child => {
        if (typeof child === 'object'){
            element.appendChild(createElement(child));
        } else {
            element.textContent = child;
        }
    });
    return element;
}
