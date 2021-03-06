* Async Pipe
```javascript  
   @Component({
     selector: 'app',
     template: `
        <h1> {{ clock | async }} </h1>
     `
   })

   export class App {
        clock = Observable.interval(1000);
        
        constructor() {
           this.clock.subscribe(console.log.bind(console)); //Async pipe do this
        }
  } 
```
   
   async pipe => subscribing and taking values.

* Render Observable with date pipes
   
   template: `
      <h1> {{ clock | async | date: 'DD-MM-YYYY'}} </h1>
   `
```javascript
    export class App {
        clock = Observable
                .interval(1000)
                .map(()=> new Date());
    }
```

   which prints date time for every seconds.

* Handling Click events with subjects
 - we can use 

  clickEventObservable$ =  Observable.fromEvent('get elem ref', 'click')
 
 - We can also use subjects.

   template: `
     <button (click)="click$.next()"> Update </button>
     <h1> {{ clock | async | date: 'DD-MM-YYYY'}} </h1>
    '
```javascript
   export class App {
         click$ = new Subject();
   
         constructor() {
            this.clock = this.click$.map(() => new Date());
         }
  }
```
   in here we can create subject. Subject is both observer and observable
 
   we can emit using "click$.next()" this emit events for subject.

   $click.map( () => new Date()) , generates observable on based on next.
* Handle Click and Intervals Together with Merge
 suppose we want click and update together we can use merge
   
template: `
     <button (click)="click$.next()"> Update </button>
     <h1> {{ clock | async | date: 'DD-MM-YYYY'}} </h1>
    '
```javascript
   export class App {
         click$ = new Subject();
         clock;

         constructor() {
            this.clock = Observable.merge(
                           this.click$,
                           Observable.interval(5000)
                         ).map( () => new Date());
         }
  }
```
   Observable.merge merges two observables together and make it into single.
* Manage State with RxJS with StartWith and Scan
```javascript
  constructor() {
    this.clock = Observable.merge(
                    this.click$,
                    Observable.interval(100)
                 )
                 .startWith (new Date())  // Start with this value
                 .scan( (acc, curr) => {
                       const date = new Date(acc.getTime());
                       date.setSecond(date.getSeconds() + 1);
                       return date;
                  });
                }
```
   startWith => gives instance to start with so no need to generate new 
                everytime.
    scan  => Update everyevents happen

                       
