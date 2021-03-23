import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const crypto = window.crypto || window.msCrypto;

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
}

const initialSeed = getRandomInts(5);
const flags = [initialSeed[0], initialSeed.slice(1)];

Elm.Main.init({
  node: document.getElementById('root'),
  flags
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

