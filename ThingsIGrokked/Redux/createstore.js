const createStore = (reducer) => {
    let state;
    let listeners = [];

    const getState = () => state;

    const dispatch = (action) => {
        state = reducer(state, action);
        console.log('Listeners => ',listeners);
        listeners.forEach( l => l());
    }

    const subscribe = (listener) => {
        listeners.push(listener);

        return () => {
            console.log('UnSubscribe listeners => ',listeners)
            listeners = listeners.filter( l => l !== listener);
            console.log('UnSubscribe After  listeners => ',listeners)

        }
    }

    dispatch({})

    return { getState, dispatch, subscribe};
}

const counter = (state = 0, action) => {
    switch(action.type) {
        case 'INCREMENT':
            return state+1;
        case 'DECREMENT':
            return state-1;
        default:
            return state;
    }
}

const store = createStore(counter);

console.log(store.getState());

const render = () => {
    console.log(store.getState());
}
const renderLine = () => {
    console.log('---------------');
    console.log(store.getState());
    console.log('---------------');
}

store.subscribe(render);
let rl = store.subscribe(renderLine);

console.log('About to Dispatch');
store.dispatch({type: 'INCREMENT'});
console.log('Dispatch Decrement');
store.dispatch({type: 'DECREMENT'});
console.log('calling unsubscribe');
rl();
