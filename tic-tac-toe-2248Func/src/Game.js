import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult } from './util';
import Block from './Block';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [generatedBlock, setValue] = useState(0);
  const pathingInProgress = path.length > 1;

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    if (newPath.length > 1){
      const pathS = JSON.stringify(newPath);
      const gridS = JSON.stringify(grid);
      const queryS = "siguiente_bloque(" + gridS + "," + pathS + "," + numOfColumns + ", BloqueResultado)";
      pengine.query(queryS, (success, response) => {
        if (success) {
          setValue(response['BloqueResultado']);
        }
      });
    }
    else{
      setValue(0);
    }
    setPath(newPath);
    console.log(JSON.stringify(newPath));
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  function toBoost() {
    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  function helpMaxMoves() {
    const gridS = JSON.stringify(grid);
    const queryS = "ayuda_movida_maxima(" + gridS + "," + numOfColumns + ", RPath)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        console.log(response['RPath']);
        onPathChange(response['RPath']);
      } else {
        setWaiting(false);
      }
    });
    setWaiting(false);
  }

  function adyacentMaxMoves() {
    const gridS = JSON.stringify(grid);
    const queryS = "ayuda_maximos_iguales_adyacentes(" + gridS + "," + numOfColumns + ", RPath)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        console.log(response['RPath']);
        onPathChange(response['RPath']);
      } else {
        setWaiting(false);
      }
    });
    setWaiting(false);
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 200);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
      {pathingInProgress ? (
        <Block color={generatedBlock}>{generatedBlock}</Block>
      ) : (
        <div className="score">{score}</div>
      )}
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <div className = "button">
        {waiting ?
          <button className="boostButtonDisabled">Booster</button>
          :
          <button className="boostButton" onClick = {toBoost} 
          disabled={waiting ? true : false}>Booster</button>
        }
        {waiting ?
          <button className="boostButtonDisabled">Max Move</button>
          :
          <button className="boostButton" onClick = {helpMaxMoves} 
          disabled={waiting ? true : false}>Max Move</button>
        }
        {waiting ?
          <button className="boostButtonDisabled">Adyacent</button>
          :
          <button className="boostButton" onClick = {adyacentMaxMoves} 
          disabled={waiting ? true : false}>Adyacent</button>
        }
      </div>
    </div>
  );
}

export default Game;